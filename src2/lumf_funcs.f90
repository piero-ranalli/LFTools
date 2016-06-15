module lumf_funcs

! luminosity function theoretical description: double powerlaws, ldde, etc.

! We define a base type luminosityfunction which includes (via
! composition) a z0function and an evolution, and provides a generic
! lfcalc(L,z) covering common cases. Then we start inheriting from it.
!
! Types doublepowerlaw, lade and ldde are defined.
!
! SYNPOSIS:
!
! class(luminosityfunction) :: lf
! class(doublepowerlaw) :: dp
! class(lddevol) :: ldde
! lf%z0func => dp
! lf%evol => ldde
! call lf%z0func%set( ... )
! call lf%evol%set( ... )
! fi = lf%lfcalc(Lx,z)

use precision

implicit none


! base evolution type, with dummy procedures for no-evolution case
type :: evolution
 contains
   procedure :: calcl => dummy_evol_calcl
   procedure :: calcd => dummy_evol_calcd
   procedure :: priorprob => dummy_evol_priorprob
end type evolution

! base z=0 LF type, defining an interface for its calculation
type, abstract :: z0function
 contains
   procedure :: calc => dummy_z0function_calc
   procedure :: priorprob => dummy_z0function_priorprob
end type z0function
! type, abstract :: z0function
!  contains
!    procedure(z0f_calc_interf), deferred :: calc
! end type z0function

! abstract interface
!    real function z0f_calc_interf (this,Lx)
!      import z0function
!      class(z0function) :: this
!      real,intent(in) :: Lx
!    end function z0f_calc_interf
! end interface

! base type from which we inherit all other LF, which includes pointers to
! z0func and evolution
type :: luminosityfunction
   class(z0function),allocatable :: z0func
   class(evolution), allocatable :: evol
 contains
   procedure :: lfcalc => generic_lfcalc
   !procedure(like_interf), deferred :: likelihood
end type luminosityfunction



! now the "real-world" types
type, extends(z0function)  :: doublepowerlaw
   real(kind=rkind) :: A,gamma1,gamma2,Lstar
 contains
   procedure :: set  => doublepowerlaw_set
   procedure :: calc => doublepowerlaw_calc
end type doublepowerlaw



! no evolution: dummy class, needed only to answer 'select type' statements
type, extends(evolution) :: noevol
end type noevol


! PLE+PDE = PDLE
type, extends(evolution) :: pdlevol
   real(kind=rkind) :: etad, etal
 contains
   procedure :: set => pdlevol_set
   procedure :: calcl => pdlevol_calcl
   procedure :: calcd => pdlevol_calcd
end type pdlevol

! LDDE family
type, extends(evolution) :: lddevol
   real(kind=rkind) :: zc, p1, p2, alfa, La
 contains
   procedure :: set  => lddevol_set
   procedure :: calcd => lddevol_calcd
end type lddevol

type, extends(evolution) :: ldde15evol  ! Ueda et al. 2015
   real(kind=rkind) :: zc1, p1star, beta1, Lp, p2, alfa1, La1, zc2, p3, alfa2, La2
 contains
   procedure :: set  => ldde15evol_set
   procedure :: calcd => ldde15evol_calcd
end type ldde15evol


! LADE family
type, extends(evolution) :: ladevol
   real(kind=rkind) :: zc, p1, p2, d
 contains
   procedure :: set  => ladevol_set
   procedure :: calcl => ladevol_calcl
   procedure :: calcd => ladevol_calcd
end type ladevol

type, extends(ladevol) :: ladevol_ueda
 contains
   procedure :: calcl => ladevol_ueda_calcl
end type ladevol_ueda


contains


pure elemental real(kind=rkind) function ladevol_ueda_calcl (this,Lx,z)  result (el)
  class(ladevol_ueda),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z
  real(kind=rkind) :: t,norm

  if (z <= this%zc) then
     el = this%p1*log10(1+z)
  else
     el = this%p1*log10(1+this%zc) + this%p2*log10((1+z)/(1+this%zc))
  end if
end function ladevol_ueda_calcl





! no-evolution procedures (expected to be overridden)
pure elemental function dummy_evol_calcl (this,Lx,z)
  real(kind=rkind) :: dummy_evol_calcl
  class(evolution),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z
  dummy_evol_calcl = 0
end function dummy_evol_calcl

pure elemental function dummy_evol_calcd (this,Lx,z)
  real(kind=rkind) :: dummy_evol_calcd
  class(evolution),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z
  dummy_evol_calcd = 0
end function dummy_evol_calcd

pure elemental function dummy_z0function_calc (this,Lx)
  real(kind=rkind) :: dummy_z0function_calc
  class(z0function),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx
  dummy_z0function_calc = 0
end function dummy_z0function_calc

function dummy_evol_priorprob (this)
  real(kind=rkind) :: dummy_evol_priorprob
  class(evolution) :: this
  dummy_evol_priorprob = 0  ! log(1)
end function dummy_evol_priorprob

function dummy_z0function_priorprob (this)
  real(kind=rkind) :: dummy_z0function_priorprob
  class(z0function) :: this
  dummy_z0function_priorprob = 0  ! log(1)
end function dummy_z0function_priorprob

! generic luminosity function calculation, covering common cases
! (PLE,PDE,LADE,LDDE,ILDE,...)
pure elemental function generic_lfcalc  (this,Lx,z)  result (lf)
  real(kind=rkind) :: lf
  class(luminosityfunction),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z

  lf = this%z0func%calc( Lx - this%evol%calcl(Lx,z) ) + this%evol%calcd(Lx,z)
end function generic_lfcalc

! real function dummy_likelihood (this)  result (l)
!   class(doublepowerlaw) :: this
!   l=1
! end function dummy_likelihood


! procedures for z0func doublepowerlaw
subroutine doublepowerlaw_set(this,dA,dgamma1,dgamma2,dLstar)
  class(doublepowerlaw) :: this
  real(kind=rkind),intent(in) :: dA,dgamma1,dgamma2,dLstar

  this%A = dA
  this%gamma1 = dgamma1
  this%gamma2 = dgamma2
  this%Lstar = dLstar
end subroutine doublepowerlaw_set


pure elemental function doublepowerlaw_calc (this,Lx)
  real(kind=rkind) doublepowerlaw_calc
  class(doublepowerlaw),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx
  real(kind=rkind) :: x,y,t

  ! this is dFi/dLogX in Ebrero+2009
  !doublepowerlaw_calc = -6.+log10(this%A)          &
  !     - log10( 10.**((Lx-this%Lstar)*this%gamma1) &
  !     + 10.**((Lx-this%Lstar)*this%gamma2) )
  
  ! save one exponential calculation per call:
  ! (and avoid overflows when there is luminosity evolution)

  ! this%A is now in log-scale
  t = Lx-this%Lstar
  x = t*this%gamma1
  y = t*this%gamma2
  if (x>y) then
     doublepowerlaw_calc = -6.+this%A          &
          - x - log10(1. + 10.**(y-x))
  else
     doublepowerlaw_calc = -6.+this%A          &
          - y - log10(1. + 10.**(x-y))
  end if



end function doublepowerlaw_calc



! procedures for PLE & PDE
subroutine pdlevol_set(this, detad, detal)
  class(pdlevol) :: this
  real(kind=rkind),intent(in) :: detal, detad

  this%etad = detad
  this%etal = detal
end subroutine pdlevol_set


pure elemental function pdlevol_calcd (this,Lx,z)  result (pde)
  real(kind=rkind) :: pde
  class(pdlevol),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z

  pde = this%etad*log10(1+z)
end function pdlevol_calcd

pure elemental function pdlevol_calcl (this,Lx,z)  result (ple)
  real(kind=rkind) :: ple
  class(pdlevol),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z

  ple = this%etal*log10(1+z)
end function pdlevol_calcl



! procedues for LDDE
subroutine lddevol_set(this,dzc, dp1, dp2, dalfa, dLa)
  class(lddevol) :: this
  real(kind=rkind),intent(in) :: dzc, dp1, dp2, dalfa, dLa

  this%zc = dzc
  this%p1 = dp1
  this%p2 = dp2
  this%alfa = dalfa
  this%La = dLa
end subroutine lddevol_set


subroutine ldde15evol_set(this, dzc1, dp1star, dbeta1, dLp, dp2, &
                          dalfa1, dLa1, dzc2, dp3, dalfa2, dLa2)
  class(ldde15evol) :: this
  real(kind=rkind),intent(in) :: dzc1, dp1star, dbeta1, dLp, dp2, &
                                 dalfa1, dLa1, dzc2, dp3, dalfa2, dLa2

  this%zc1    = dzc1
  this%p1star = dp1star
  this%beta1  = dbeta1
  this%Lp     = dLp
  this%p2     = dp2
  this%alfa1  = dalfa1
  this%La1    = dLa1
  this%zc2    = dzc2
  this%p2     = dp3
  this%alfa1  = dalfa2
  this%La1    = dLa2
end subroutine ldde15evol_set


pure elemental function ldde15evol_calcd (this,Lx,z)  result (ldde)
  real(kind=rkind) :: ldde
  class(ldde15evol),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z
  real(kind=rkind) :: zclx1,zclx2,p1

  if (Lx >= this%La1) then   ! lum-dependent zcut
     zclx1 = this%zc1
  else
     zclx1 = this%zc1 * 10.**(this%alfa1*(Lx-this%La1))
  endif

  if (Lx >= this%La2) then   ! lum-dependent zcut
     zclx2 = this%zc2
  else
     zclx2 = this%zc2 * 10.**(this%alfa2*(Lx-this%La2))
  endif

  p1 = this%p1star + this%beta1*(Lx-this%Lp)

  if (z <= zclx1) then
     ldde = p1*log10(1.+z)
  else if (z <= zclx2) then
     ldde = p1*log10(1.+zclx1) + this%p2*log10( (1.+z)/(1.+zclx1) )
  else
     ldde = p1*log10(1.+zclx1) + this%p2*log10( (1.+zclx2)/(1.+zclx1) ) &
                               + this%p3*log10( (1.+z)    /(1.+zclx2) )
  endif
end function ldde15evol_calcd



pure elemental function lddevol_calcd (this,Lx,z)
  real(kind=rkind) lddevol_calcd
  class(lddevol),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z
  real(kind=rkind) :: zclx

  if (Lx >= this%La) then   ! lum-dependent zcut
     zclx = this%zc
  else
     zclx = this%zc * 10.**(this%alfa*(Lx-this%La))
  endif

  if (z <= zclx) then
     lddevol_calcd = this%p1*log10(1.+z)
  else
     lddevol_calcd = this%p1*log10(1.+zclx) + this%p2*log10( (1.+z)/(1.+zclx) )
  endif
end function lddevol_calcd


! procedures for LADE
subroutine ladevol_set(this,dzc, dp1, dp2, dd)
  class(ladevol) :: this
  real(kind=rkind),intent(in) :: dzc, dp1, dp2, dd

  this%zc = dzc
  this%p1 = dp1
  this%p2 = dp2
  this%d = dd
end subroutine ladevol_set


pure elemental real(kind=rkind) function ladevol_calcl (this,Lx,z)  result (el)
  class(ladevol),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z
  real(kind=rkind) :: t,norm

  t = (1+this%zc)/(1+z)

  ! normalize the evolution so that el(z=0)=1, following Fotopoulou et al.
  ! (NB Aird 2010 does not normalize)
  norm = (1+this%zc)**this%p1 + (1+this%zc)**this%p2

  el = -log10 ( (t**this%p1 + t**this%p2) / norm )
end function ladevol_calcl

pure elemental real(kind=rkind) function ladevol_calcd (this,Lx,z)  result (led)
  class(ladevol),intent(in) :: this
  real(kind=rkind),intent(in) :: Lx,z

  led = this%d*(1+z)
end function ladevol_calcd





end module lumf_funcs

