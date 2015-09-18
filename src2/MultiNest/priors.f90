module priors

! define routines for getting flat and cauchy priors, to be used in
! mn_like.f90

use precision

implicit none

type, abstract :: tprior
   real(kind=rkind), pointer :: params(:,:)
 contains
   procedure(sp), deferred :: scalepar
end type tprior

abstract interface
   subroutine sp(this, cube, scaled)
     import tprior,rkind
     class(tprior) :: this
     real(kind=rkind), intent(in)  :: cube(:)
     real(kind=rkind), intent(out) :: scaled(:)
   end subroutine sp
end interface



type, extends(tprior) :: flatprior
 contains
   procedure :: scalepar => scaleflat
end type flatprior

type, extends(tprior) :: cauchygammaprior
 contains
   procedure :: scalepar => scalecauchygamma
end type cauchygammaprior

contains

subroutine scaleflat (this, cube, scaled)
  class(flatprior) :: this
  real(kind=rkind), intent(in) :: cube(:)
  real(kind=rkind), intent(out) :: scaled(:)

  ! simply rescale uniformly-distributed priors
  scaled(:) = (this%params(:,2)-this%params(:,1))*cube(:) + this%params(:,1)

  ! the line above corresponds to the following from the MultiNest examples:
  !scaledparams(i)=(spriorran(i,2)-spriorran(i,1))*Cube(i)+spriorran(i,1)
end subroutine scaleflat


subroutine scalecauchygamma (this, cube, scaled)
  class(cauchygammaprior) :: this
  real(kind=rkind), intent(in) :: cube(:)
  real(kind=rkind), intent(out) :: scaled(:)

  where (this%params(:,3)>0)  ! is Cauchy
     ! first, transform uniformly-distributed variable into Cauchy-distr:
     scaled(:) = tan( 3.1415927d0*(cube(:) - .5d0))
     ! second, introduce location and scale:
     scaled(:) = this%params(:,1) + scaled(:)*this%params(:,2)
  elsewhere  ! is Gamma(k=1,theta=1)
     scaled(:) = -log( cube(:) ) * this%params(:,1)
  end where

end subroutine scalecauchygamma



end module priors
