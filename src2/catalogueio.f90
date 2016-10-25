module catalogueio
use precision
use areas
use photoz

integer, parameter :: maxcatsize = 1300000

!> the 'catalogue' type contains the following vectors:
!! id = identification number
!! Lx = luminosity
!! z  = redshit
!! weight = probability of the id source having Lx and z
!! includeprob = probability for the inclusion of the source in the catalogue, it is the
!!               match probability used in lf-catcorrect and lf-binned
type catalogue
   integer :: size
   integer,dimension(:),allocatable :: id
   real(kind=rkind),   dimension(:), allocatable :: Lx, z, weight, includeprob
   type(coverage) :: area
 contains
   procedure :: coverage => catcoverage
   procedure :: readcat, readarea, &
                read_lss_cdfs_areas, read_only_cdfs_area,  &
                read_only_lss_area, read_only_cosmos_area, write
end type catalogue


! type, extends(catalogue) :: cataloguebayes
!    type(photoz_pdf), dimension(maxcatsize) :: zpdf
!    real(kind=rkind), dimension(maxcatsize) :: flux, opticalid
!  contains
!    procedure :: read => cataloguebayes_read
! end type cataloguebayes


contains

real function catcoverage(this, f)
  class(catalogue) :: this
  real(kind=rkind) :: f

  catcoverage = this%area%interpolate(f)
end function catcoverage


subroutine readcat (this, filename, zmin,zmax,lmin,lmax)
  use fileutils

  class(catalogue) :: this
  character(*)  filename
  real(kind=rkind), intent(in) :: zmin,zmax,lmin,lmax

  character(120) :: row,row2
  integer u,i,catsize
  real(kind=rkind) :: foo

  catsize = lines_in_file( filename, .true. )
  
  allocate( this%id(catsize), this%Lx(catsize),       &
            this%z(catsize),  this%weight(catsize),   &
            this%includeprob(catsize)                   )

  write (*,*) catsize
  open (newunit=u, file=filename, status='old')
  i=1
  do
     read (u,'(A)',end=999) row
     if (iscomment(row))  cycle

     ! check before actually putting data in memory - but this should never happen
     if (i>catsize)  then
        write (*,*) i
        write (*,*) 'reached maximum size of catalogue arrays - something is wrong'
        stop
     end if

     read (row,*) this%id(i), foo, this%weight(i), this%z(i), this%Lx(i), this%includeprob(i)

     ! skip sources outside limits
     if (this%z(i)  < zmin .or. this%z(i)  > zmax .or.  &
         this%Lx(i) < lmin .or. this%Lx(i) > lmax)      &
           cycle
     
     if (this%Lx(i) > 48) then
        write (*,*) 'error at line ',i
        stop
     end if

     i = i+1

  end do
  close (u)
  
999 this%size = i-1

end subroutine readcat


subroutine write (this)
  class(catalogue) :: this
  integer u,i
  real foo

  open (newunit=u, file='catalogue-rebin.dat', status='new')
  do i=1,this%size
     write (u,*) this%id(i), 0, this%weight(i), this%z(i), this%Lx(i), this%includeprob(i)
  end do
end subroutine write


! subroutine cataloguebayes_read (this, filename)
!   class(cataloguebayes) :: this
!   character*(*)  filename
!   integer u,i
!   real foo

!   open (newunit=u, file='catalogue.dat', status='old')
!   i=1
!   do
!      read (u,*,end=999) this%id(i), this%flux(i), this%opticalid(i), this%z(i)
!      ! load photoz pdf info
!      i = i+1
!   end do

! 999 this%size = i-1

! end subroutine cataloguebayes_read

subroutine readarea (this,area)
  class(catalogue) :: this
  character*(*) :: area

  call this%area%readgeneric(area)
end subroutine readarea



subroutine read_lss_cdfs_areas (this)
  class(catalogue) :: this
  type(coverage) :: areacdfs

  call this%area%set_corrected_area(.true.)
  call this%area%readlss
  call areacdfs%readcdfs
  call this%area%add(areacdfs)
end subroutine read_lss_cdfs_areas

subroutine read_only_cdfs_area (this)
  class(catalogue) :: this

  call this%area%readcdfs
end subroutine read_only_cdfs_area

subroutine read_only_lss_area (this)
  class(catalogue) :: this

  call this%area%set_corrected_area(.true.)
  call this%area%readlss
end subroutine read_only_lss_area

subroutine read_only_cosmos_area (this)
  class(catalogue) :: this

  call this%area%readcosmos
end subroutine read_only_cosmos_area


end module catalogueIO
