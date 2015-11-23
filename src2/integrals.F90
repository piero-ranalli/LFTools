! module for integration of generic function
!
! should allow different implementation of integration, i.e. with rectangles
! or with Gauss rules from GSL or CERNLIB

module integrals

use precision
implicit none

interface integrate
!   module procedure integrate_rectangle
   module procedure integrate_trapezoidal,integrate_trapezoidal2
!   module procedure integrate_trapezoidal,integrate_QNG2
end interface integrate


abstract interface
   function integrand(x,y)
     real(kind=8) :: integrand
     real(kind=8), intent(in) :: x,y
   end function integrand
end interface

real(kind=rkind), private :: cuba_limits(4)


! abstract interface
!    integer function cuhre_helper(ndim,xx,ncomp,ff)
!      integer :: ndim,ncomp
!      double precision :: xx(*),ff(*)
!    end function cuhre_helper
! end interface


!procedure :: scaledintegrand,fcn

contains


recursive function integrate_rectangle (f, from, to) result (sum)

  interface
     function f(x)
       real(kind=8) :: f
       real(kind=8), intent(in) :: x
     end function f
  end interface

  real(kind=rkind), intent(in) :: from, to

  integer, parameter :: nsteps = 100
  integer i
  real(kind=rkind) :: step, sum

  step = (to-from)/nsteps
  sum = 0.
  do i = 0, nsteps-1
     sum = sum + f(from+step*(i+.5))
  end do
  sum = sum * step

end function integrate_rectangle


recursive function integrate_trapezoidal (f, from, to) result (integral)

  interface
     function f(x)
       real(kind=8) :: f
       real(kind=8), intent(in) :: x
     end function f
  end interface

  real(kind=rkind), intent(in) :: from, to
  real(kind=rkind) :: integral, step

  integer, parameter :: nsteps = 100
  integer i
  real(kind=rkind) :: xval(0:nsteps),yval(0:nsteps),tmp

  step = (to-from)/nsteps
  xval = (/ (from+step*i, i=0, nsteps)  /)
  yval = (/ (f(xval(i)),  i=0, nsteps)  /)

  tmp = sum(yval(1:nsteps-1))
  integral = ((yval(0)+yval(nsteps))*.5d0 + tmp) * step
  !integral = (sum(yval(0:nsteps-1))+sum(yval(1:nsteps))) * .5 * step

end function integrate_trapezoidal



recursive function integrate_trapezoidal2 (f, from, to, param) result (integral)
! like above but with an additional parameter

  interface
     function f(x,y)
       real(kind=8) :: f
       real(kind=8), intent(in) :: x,y
     end function f
  end interface

  real(kind=rkind), intent(in) :: from, to, param
  real(kind=rkind) :: integral, step, tmp

  integer, parameter :: nsteps = 100
  integer i
  real(kind=rkind) :: xval(0:nsteps),yval(0:nsteps)

  step = (to-from)/nsteps
  xval = (/ (from+step*i,       i=0, nsteps)  /)
  yval = (/ (f(xval(i),param),  i=0, nsteps)  /)

  tmp = sum(yval(1:nsteps-1))
  integral = ((yval(0)+yval(nsteps))*.5d0 + tmp) * step
  !integral = (sum(yval(0:nsteps-1))+sum(yval(1:nsteps))) * .5 * step

end function integrate_trapezoidal2



#ifdef CUBA

function integrate_cuhre_2d (f, from1, to1, from2, to2) result (res)
  ! integrate using CUBA
  procedure(integrand) :: f
  real(kind=rkind), intent(in) :: from1, to1, from2, to2
  real(kind=rkind) :: res

  integer, parameter :: ndim = 2
  integer, parameter :: ncomp = 1
  integer, parameter :: userdata = 0
  integer, parameter :: nvec = 1
  real(kind=8), parameter :: epsrel = 1D-3
  real(kind=8), parameter :: epsabs = 1D-5
  integer, parameter :: verbose = 0
  integer, parameter :: mineval = 0
  integer, parameter :: maxeval = 50000
  integer, parameter :: key = 0
  character*(1), parameter :: statefile = ""
  integer, parameter :: spin = -1
  integer :: nregions, neval, fail
  real(kind=8) :: integral(ncomp),error(ncomp),prob(ncomp)
 
  external :: cuhre

  !pinteg => f
  cuba_limits = [from1,to1,from2,to2]

  !write (*,*) 'cuba_limits',cuba_limits
  !write (*,*) 'lower left: ',pinteg(from1,from2)

  call cuhre(ndim, ncomp, scaledintegrand, userdata, nvec,   &
          epsrel, epsabs, verbose,               &
          mineval, maxeval, key,                 &
          statefile, spin,                       &
          nregions, neval, fail, integral, error, prob)


  res = integral(1)

contains


integer function scaledintegrand(ndim, x, ncomp, result)
  implicit none
  integer ndim, ncomp
  real(kind=rkind) :: x(ndim), result(ncomp)

  real(kind=rkind) :: range, jacobian, scaledx(ndim)

  range = cuba_limits(2) - cuba_limits(1)
  jacobian = range
  scaledx(1) = cuba_limits(1) + x(1)*range

  range = cuba_limits(4) - cuba_limits(3)
  jacobian = jacobian*range
  scaledx(2) = cuba_limits(3) + x(2)*range

  !write (*,*) 'scaled=',scaledx
  result(1) = f(scaledx(1),scaledx(2)) * jacobian

  scaledintegrand = 0

end function scaledintegrand


  
end function integrate_cuhre_2d

#endif




end module integrals
