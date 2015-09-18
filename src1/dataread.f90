module dataread

! SYNOPSYS
!
! type(catalogue) :: mycat
! mycat%read
! mycat%flux2lum
! mycat%selectprob(p)
! mycat%selectlum(l1,l2)
! mycat%selectz(z1,z2)
! mycat%correct_w_fluxes  ! this xor one of the the next two
! mycat%correct_w_hr
! mycat%correct_w_hr_nh
! n = mycat%count     ! objects in bin (integer)
! a = mycat%wcount    ! weighted count (real)
! mycat%write_lnls

! This module does two things:
! read/write catalogues
! correct them for photo-z and absorption



use cosmology
use lumfunc
use binarysearch
use photoz

implicit none

integer, parameter, private :: probRsize = 80
integer, parameter, private :: probUsize = 40
integer, parameter, private :: probzsize = 121

real, private :: gamma


type catalogue
   real, dimension(:), allocatable :: flux,zp,prob,hr,fratio,ra,dec, &  ! read from file
                                      weight,lum,weightcopy  ! assigned later
   integer, dimension(:), allocatable :: id,opticalid,zspecflag
   integer :: last, size
   logical :: correct4likelihood = .false.
   
   real, dimension(probUsize,probRsize,probzsize), private :: probmatrix
   real, dimension(probRsize), private     :: problogR
   real, dimension(probUsize), private     :: problogU
   real, dimension(probzsize), private     :: probz
 contains
   procedure :: new      => catalogue_allocate
   procedure :: destroy  => catalogue_deallocate
   procedure :: read     => catalogue_read
   procedure :: readprocessed     => catalogue_readprocessed
   procedure :: setkcorr => catalogue_setkcorr
   procedure :: selectprob => catalogue_selectprob
   procedure :: selectz    => catalogue_selectz
   procedure :: selectlum  => catalogue_selectlum
   procedure :: set_likelihood_completeness => catalogue_set_likelihood_completeness
   procedure :: correct_w_fluxes => catalogue_correct_w_fluxes
   procedure :: read_probmatrix => catalogue_read_probmatrix
   procedure :: correct_w_hr_nh_prob  => catalogue_correct_w_hr_nh_prob
   procedure :: correct_w_photozpdf   => catalogue_correct_w_photozpdf
   procedure :: copycat => catalogue_copycat
   procedure :: reset  => catalogue_reset

!   procedure :: count  => catalogue_count
   procedure :: wcount => catalogue_wcount

   procedure :: writelnls => catalogue_writelnls

   procedure :: flux2lum

   procedure :: uprob => catalogue_uprob
   procedure :: saveweights => catalogue_saveweights
   procedure :: recoverweights => catalogue_recoverweights
end type catalogue

contains

  subroutine catalogue_allocate (this,size)
    class(catalogue) :: this
    integer, intent(in) :: size
    integer :: status

    allocate( this%flux(size),this%zp(size),this%prob(size),this%hr(size),this%fratio(size),this%ra(size),this%dec(size), &
              this%weight(size),this%lum(size), this%id(size), this%opticalid(size),this%zspecflag(size), &
              STAT = status )

    if (status /= 0)   stop  "Not enough memory to allocate all arrays"

    this%size = size
  end subroutine catalogue_allocate

  subroutine catalogue_deallocate (this)
    class(catalogue) :: this

    deallocate( this%flux,this%zp,this%prob,this%hr,this%fratio,this%ra,this%dec, &
         this%weight,this%lum, this%id, this%opticalid,this%zspecflag )
  end subroutine catalogue_deallocate

  subroutine catalogue_reset (this)
    class(catalogue) :: this

    this%last = 0
  end subroutine catalogue_reset

  subroutine catalogue_read (this,infile)
    class(catalogue) :: this
    integer u,i
    character(*) :: infile
    character(3) :: foo

    open (newunit=u, file=infile, status='old')

    ! skip first line (should be a header)
    read (u,*) foo

    i = 1
    do
       read (u,*,end=5)  this%id(i),this%flux(i),this%zspecflag(i),this%zp(i),this%prob(i),this%fratio(i), &
                         this%opticalid(i)

       if (this%zp(i) <= 0) then
          write (*,*) 'redshift ',this%zp(i),' of source ',i,' is <=0'
       end if
       
       this%weight(i) = 1.
       i = i+1
    enddo

5   close(unit=u)
    this%last = i-1

    write (*,*) this%last,' sources read from catalogue'

  end subroutine catalogue_read

  subroutine catalogue_setkcorr(this,mygamma)
    class(catalogue) :: this
    real,intent(in) :: mygamma

    gamma = mygamma
  end subroutine catalogue_setkcorr

  
  impure elemental real function f2l(x,z)
    !real, parameter :: gamma = 1.4
    real, intent(in) :: z,x
    !  f2l(x) = log10(x) + log10(4.*3.1416) + 2.d0*log10(3.086 * lumd(this%zp)) + 48. + &
    !       ! la correzione K:  
    !       (-2.d0+gamma)*log10(1.+this%zp)
    f2l = log10(x) + 50.07800293 + 2.*log10( lumd(z) ) + (-2.d0+gamma)*log10(1.+z)
  end function f2l


  subroutine flux2lum (this)
    class(catalogue) :: this

    where( this%zp > 0 )
       this%lum = f2l(this%flux,this%zp)
    elsewhere
       this%lum = 0
       this%weight = 0
    end where
  end subroutine flux2lum


  subroutine catalogue_selectprob (this,prob)
    class(catalogue) :: this
    real, intent(in) :: prob

    where ( this%prob > prob )
       this%weight = 0
    end where
  end subroutine catalogue_selectprob

  subroutine catalogue_selectz (this,z1,z2)
    class(catalogue) :: this
    real, intent(in) :: z1,z2

    where ( this%zp <= z1 .or. this%zp > z2 )
       this%weight = 0
    end where
  end subroutine catalogue_selectz

  subroutine catalogue_selectlum (this,l1,l2)
    class(catalogue) :: this
    real, intent(in) :: l1,l2

    where ( this%lum <= l1 .or. this%lum > l2 )
       this%weight = 0
    end where
  end subroutine catalogue_selectlum



  subroutine catalogue_correct_w_fluxes (this)
    class(catalogue) :: this
    integer u,i
    type(curve) :: correction
    character*3 foo
    real corr

    ! ------------COMPLETENESS CORRECTIONS-----------------
    ! use the following file for testing purposes
    !open (newunit=u, file='completeness-corrections.dat',status='old')
    open (newunit=u, file='completeness-fluxcorrections.dat',status='old')

    ! salta la prime 11 righe che sono di commento
1   format (A)
!    do i=1,11
       read (u,1) foo
!    enddo

    do i = 1,150
       read (u,*,end=4040) correction%x(i), correction%y(i)
    enddo
4040 continue
    correction%last = i-1

    close(unit=u)

    ! doesn't work:
    !where (this%weight > 0)
    !   this%weight = this%weight + 10.**(-(correction%interpolate(log10(this%flux))))
    !end where
    do i=1,this%last
       if (this%weight(i) > 0) then
          corr = correction%interpolate(log10(this%flux(i)))
          ! use this line for testing purposes:
          !this%weight(i) = this%weight(i) * 10.**(-corr)

          if (this%correct4likelihood) then
             this%weight(i) = this%weight(i) * corr
          else
             this%weight(i) = this%weight(i) / corr
          endif
          !write(19,*) this%weight(i)
       end if
    end do
    !close(19)
  end subroutine catalogue_correct_w_fluxes


  subroutine catalogue_set_likelihood_completeness (this)
    class(catalogue) :: this

    this%correct4likelihood = .true.
  end subroutine catalogue_set_likelihood_completeness


  real function catalogue_wcount(this)   result(tot)
    class(catalogue), intent(in) :: this

    tot = sum(this%weight)
  end function catalogue_wcount





  subroutine catalogue_read_probmatrix (this)
    class(catalogue) :: this
    integer u,i,j,k


    open (newunit=u, file='RUzmatrix.dat',status='old')
    ! format of the above file:  
    ! 1st line, logR (80 of them)
    ! 2nd line, logU (40 of them)
    ! 3rd line, z (121 of them)
    ! 4th-last line, 121 matrices of probabilities: each of 40 cols, 80 lines 

    read (u,*,end=4042) (this%problogR(j), j=1,probRsize)
    read (u,*,end=4042) (this%problogU(i), i=1,probUsize)
    read (u,*,end=4042) (this%probz(k), k=1,probzsize)
    ! write (*,*) 'uno'
    ! write (*,*)  (this%problogU(k), k=1,40)
    ! write (*,*) 'due'
    ! write (*,*)  (this%problogR(k), k=1,80)
     ! write (*,*) 'tre'
     ! write (*,*)  (this%probz(k), k=1,121)
     ! write (*,*) 'quattro'


    do k = 1,probzsize
       do j = 1,probRsize
          read (u,*,end=4042) (this%probmatrix(i,j,k),i=1,probUsize)
          ! write (*,*) (this%probmatrix(i,j,k),i=1,40)
       enddo
    enddo

    return

4042 write (*,*) 'premature end of file RUmatrix.dat'
    stop

  end subroutine catalogue_read_probmatrix



  subroutine catalogue_correct_w_hr_nh_prob (this,catcorr)
    class(catalogue) :: this,catcorr
    type(curve) :: correction
    character*3 foo
    real corr,corrprob,logR,z
    integer i,j,row,page,k
    !integer, external :: locatr



    catcorr%last = 0

    do i=1,this%last
       if (this%weight(i) > 0) then
          if (this%fratio(i) > 1.e4)   then      ! this path never going to be used anyway as it's unphysical
             !write (*,*) 'unphysical path'
             call this%copycat(i,catcorr,1.,1.)  
          else
             if (this%fratio(i) < 1.e-4) then    ! set boundary for very hard spectra
                this%fratio(i) = 1.e-4
                !write (*,*) 'low ratio'
             endif

             logR = log10( this%fratio(i) )
             z = this%zp(i)

             ! find which page in the matrix (redshift) has to be used
             !page = locatr(this%probz,probzsize,z)       ! (cernlib)
             !write (*,*) 'page from locatr: ',page
             page = nearest(this%probz,z)
             !write (*,*) 'page from nearest: ',page
     ! write (*,*) 'sette'
     ! write (*,*)  (this%probz(k), k=1,121)
     ! write (*,*) 'otto'
             !page = abs(page)   ! read the short writeup...

             ! find which row (R) in the matrix
             !row = locatr(this%problogR,probRsize,logR)  ! (cernlib)
             row = nearest(this%problogR,logR)

             if (row==0) then
                row = 1
             endif

             !write (*,*) z,page,row

             ! correct for any U with its probability
             do j=1,probUsize
                corrprob = this%probmatrix(j,row,page)
                if (corrprob>0) then
                   corr = this%problogU(j)
                   !write (*,*) corr, corrprob
                   call this%copycat(i,catcorr,corrprob,corr)
                endif
             enddo
          endif
       end if
    end do


  end subroutine catalogue_correct_w_hr_nh_prob


  subroutine catalogue_copycat (this,i,that,weightcorr,lumcorr)
    class(catalogue) :: this, that
    integer :: i
    real :: weightcorr,lumcorr  ! lumcorr is given in log scale (it is actually problogU)

    that%last = that%last + 1

    !if (that%last > cataloguesize) then
    if (that%last > that%size) then
       write (*,*) 'maximum internal catalogue size reached. help.'
       write (*,*) 'that%last=',that%last,' this%size=',this%size
       
       stop
    endif

    that%id(that%last)     = this%id(i)
    that%flux(that%last)   = this%flux(i) * 10**lumcorr
    that%zp(that%last)     = this%zp(i)
    that%prob(that%last)   = this%prob(i)
    that%hr(that%last)     = this%hr(i)
    that%fratio(that%last) = this%fratio(i)
    that%weight(that%last) = this%weight(i) * weightcorr
    that%lum(that%last)    = this%lum(i)    + lumcorr
    that%zspecflag(that%last) = this%zspecflag(i)
    that%opticalid(that%last) = this%opticalid(i)


  end subroutine catalogue_copycat

  subroutine catalogue_writelnls (this,filename)
    class(catalogue) :: this
    character(*) :: filename
    integer i,u

    open (newunit=u, file=filename, status='new')

    do i = 1, this%last
       if (this%weight(i)<1.e-20) cycle
       write (u,*) this%id(i), this%flux(i), this%weight(i), this%zp(i), this%lum(i)
    end do

    close (unit=u)
  end subroutine catalogue_writelnls



  subroutine catalogue_readprocessed (this,filename)
    class(catalogue) :: this
    character(*) :: filename
    integer i,u

    open (newunit=u, file=filename, status='old')

    i = 1
    do
       read (u,*,end=555) this%id(i), this%flux(i), this%weight(i), this%zp(i), this%lum(i)
       i = i+1
    end do

555 close (unit=u)
    this%last = i-1
  end subroutine catalogue_readprocessed



  subroutine catalogue_correct_w_photozpdf(this,catcorr,optical)
    class(catalogue) :: this,catcorr
    class(photoz_pdf) :: optical
    integer i,j
    real wei,z



    catcorr%last=0  ! reset catcorr

    do i = 1, this%last
       if (this%zspecflag(i) == 1) then  ! do not correct if z is spectroscopic
          call this%copycat(i,catcorr,1.,0.)          
       else
          call optical%readpdf(this%opticalid(i))
          do j = 1, optical%lastcol
             z = optical%z(j)
             wei = optical%pdf(j)
             
             if (wei > 0) then 
                call this%copycat(i,catcorr,wei,0.)
                catcorr%zp(catcorr%last) = z
             end if
          enddo
       end if
    end do
  end subroutine catalogue_correct_w_photozpdf







real function catalogue_uprob ( this, logU, redsh, logR )  result(uprob)
  class(catalogue) :: this
  real, intent(in) :: logU, redsh, logR

  real logR1
  integer :: page, row, col
  real :: x,y,z,x1,y1,z1
  ! this follows on the line of catalogue_correct_w_hr_nh_prob

          !if (this%fratio(i) > 1.e4)   then      ! this path never going to be used anyway as it's unphysical
          !   !write (*,*) 'unphysical path'
          !else
  if (logR < -4) then
     logR1 = -4    ! set boundary for very hard spectra
  else
     logR1 = logR
  endif

  ! find which page in the matrix (redshift) has to be used
  page = binsearch(this%probz,redsh)
  
  ! find which row (R) in the matrix
  row = binsearch(this%problogR,logR1)

  ! find which column (U) in the matrix
  col = binsearch(this%problogU,logU)

  ! check if we are on the upper boundaries
  if (page==probzsize)  page = probzsize-1
  if (row==probRsize)   row  = probRsize-1
  if (col==probUsize)   col  = probUsize-1

  !write (*,*) z,page,row,col

  ! scale U,R,redshift from 0 to 1 in the interpolating cell
  x = (logU  - this%problogU(col)) / (this%problogU(col+1)-this%problogU(col))
  y = (logR1 - this%problogR(row)) / (this%problogR(row+1)-this%problogU(row))
  z = (redsh - this%probz(page))   / (this%probz(page+1)  -this%probz(page))
  x1 = 1-x
  y1 = 1-y
  z1 = 1-z

  ! trilinear interpolation, http://paulbourke.net/miscellaneous/interpolation/
  uprob = this%probmatrix(col  ,row  ,page  ) * x1*y1*z1 +  &
                                                       !
          this%probmatrix(col+1,row  ,page  ) * x *y1*z1 +  &
          this%probmatrix(col  ,row+1,page  ) * x1*y *z1 +  &
          this%probmatrix(col  ,row  ,page+1) * x1*y1*z  +  &
                                                       !
          this%probmatrix(col+1,row  ,page+1) * x *y1*z  +  &
          this%probmatrix(col  ,row+1,page+1) * x1*y *z  +  &
          this%probmatrix(col+1,row+1,page  ) * x *y *z1 +  &
                                                       !
          this%probmatrix(col+1,row+1,page+1) * x *y *z 


end function catalogue_uprob


subroutine catalogue_saveweights (this)
  class(catalogue) :: this

  allocate( this%weightcopy(size(this%weight)) )
  this%weightcopy = this%weight
end subroutine catalogue_saveweights

subroutine catalogue_recoverweights (this)
  class(catalogue) :: this

  this%weight = this%weightcopy
end subroutine catalogue_recoverweights

end module dataread
