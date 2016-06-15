module photoz

! SYNOPSYS
!
! type(photoz_pdf) :: geneva
! geneva%choose( startline, stopline, path )
! geneva%readpdf(genevaid)
! zarray = pdf%z
! pdfarray = pdf%pdf

! Read photo-z probability distributions (PDFs). It expects one file
! per source, placed in a directory identified by the path
! variable. The PDF should start on startline and end on stopline. Two
! columns are expected:
! 1. redshift
! 2. probability
! Probabilities are normalized after reading.

implicit none

integer, parameter :: photozpathlen = 256   ! NB this must be the same
  ! as in m_option_parser.F90 otherwise a "There is no specific
  ! subroutine for the generic" error will appear

type photoz_pdf
   integer :: lastcol, skip, ncols
   character (len=photozpathlen) :: path
   real, dimension(:), allocatable :: z, pdf

 contains

   procedure :: choose => photoz_choose
   procedure :: readpdf => photoz_read_pdf
   procedure :: interpolate => photoz_interpolate

end type photoz_pdf

contains


  subroutine photoz_choose(this,startline,stopline,path)
    class(photoz_pdf) :: this
    integer :: startline,stopline
    character*(*) :: path

    this%skip = startline-1
    this%ncols = stopline-startline+1
    this%path = path

    allocate (this%z(this%ncols))
    allocate (this%pdf(this%ncols))
  end subroutine photoz_choose



  subroutine photoz_read_pdf(this,id)
    class(photoz_pdf) :: this
    integer, intent(in) :: id
    integer u,i
    !real, dimension(ncols) :: pdf
    real ss

    character(9) :: cid,foo

    if (id >= 1e10) then
       write (*,*) 'optical id too large: id=',id
       stop
    end if
    

1   format (I9.9)
    write (cid,1) id

    write (*,*) 'opening id=',cid
    
    open (newunit=u, file=trim(this%path)//'Id'//cid//'.spec', status='old')

    do i=1,this%skip
       read (u,*) foo
    end do

    do i=1,this%ncols
       read (u,*) this%z(i), this%pdf(i)
    end do

    close(u)

    ss = sum(this%pdf)
    if (abs(ss - 1.) > .01) then
        !print *,'PDF not normalized for source opticalid=',id,'  sum=',ss,'delta=',abs(ss-1.)
       ! renorm... (O. Ilbert said renorm is ok)
       this%pdf = this%pdf / ss
    else
       !print *,'PDF is normalized!'
    end if

    this%lastcol = this%ncols
  end subroutine photoz_read_pdf


pure function photoz_interpolate(this, xi)  result (interp)
  use binarysearch

    class(photoz_pdf), intent(in) :: this
    real, intent(in) :: xi
    real :: interp
    real :: m
    integer :: i
    
    i = bsearch(this%z,xi,1,this%ncols)

    !print *,i,xi

    if (i==this%ncols) then  ! extrapolate
       m = (this%pdf(i)-this%pdf(i-1))/(this%z(i)-this%z(i-1))
    else
       m = (this%pdf(i+1)-this%pdf(i))/(this%z(i+1)-this%z(i))
    endif

    interp = m*xi + this%pdf(i)-m*this%z(i)  ! m*xi+q
  end function photoz_interpolate


end module photoz
