module curves

use precision
use binarysearch

! an OO interface to the DIVDIF interpolation routine from cernlib
!
! SYNOPSYS
!
! type(curve) :: c
! c%x(i) = ...   for every i
! c%y(i) = ...   for every i
! c%last = n     currently not used
! result = c%interpolate( x0 )

implicit none

integer, parameter, private :: size = 300


type curve
   real(kind=rkind), dimension(size) :: x, y
   integer :: last
 contains
   procedure :: interpolate => curves_interpolate
!   procedure :: divdif => curves_interpolate_cernlib
end type curve


contains

  ! real function curves_interpolate_cernlib(this,xi)  result(interp)
  !   class(curve), intent(in) :: this   ! needs class instead of type as it's a dummy var
  !   real,intent(in) :: xi

  !   real :: divdif, tmp
  !   external divdif

  !   !write (*,*) this%y,this%x
  !   interp = divdif(this%y,this%x,this%last,xi,1)
  ! end function curves_interpolate_cernlib

pure function curves_interpolate(this, xi)  result (interp)
    class(curve), intent(in) :: this
    real(kind=rkind), intent(in) :: xi
    real(kind=rkind) :: interp
    real(kind=rkind) :: m
    integer :: i
    
    i = bsearch(this%x,xi,1,this%last)

    !print *,i,xi

    if (i==this%last) then  ! extrapolate
       m = (this%y(i)-this%y(i-1))/(this%x(i)-this%x(i-1))
    else
       m = (this%y(i+1)-this%y(i))/(this%x(i+1)-this%x(i))
    endif

    interp = m*xi + this%y(i)-m*this%x(i)  ! m*xi+q
  end function curves_interpolate

end module curves



