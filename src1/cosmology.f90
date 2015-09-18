module cosmology

  real, parameter :: c=299792.458d0
  real :: OM = .3
  real :: OL = .7
  real :: H0 = 70

contains

  subroutine setcosmology(hubble,matter,lambda)
    real,intent(in) :: hubble,matter,lambda
    
    OM = matter
    OL = lambda
    H0 = hubble
    
  end subroutine setcosmology


!$$$      subroutine getcosmology(matter,lambda,hubble)
!$$$      real,intent(out) :: matter,lambda,hubble
!$$$
!$$$      matter = OM
!$$$      lambda = OL
!$$$      hubble = H0
!$$$
!$$$      end subroutine

impure elemental real function covol(z)
  implicit none
  real, intent(in) :: z

!     covol=dV/dz (IL VOLUME COMPRESO TRA z E z+dz) !!!
!     lumd is in Mpc
!     covol in Mpc**3         

  covol = 2.998d5*lumd(z)**2 /  &
       ( H0*(1.d0+z)**3 * sqrt(OM*(1.d0+z)+OL/(1.d0+z)**2 ) )

end function covol






pure elemental real function lumd (z)
  ! Ue Li Pen's fit to the luminosity distance

  implicit none
  real, intent(in) :: z
  real s,s3,a,as,a3,eta1,eta2,tmp
  real, parameter :: onethird = 1./3.
  real, parameter :: oneeight = -1./8.
     
  real x,x3,eta
  eta(x,x3) = 1. +x*(-.1540 + x*.4304) +x3*(.19097 +x*.066941)
  
  s3  = ((1.-OM)/OM)

  s = s3**onethird
  tmp = 2.*sqrt(s3+1)
  eta1 = eta(s,s3) ** oneeight

  a=1./(1.+z)
  a3 = a**3
  eta2 = (eta(a*s,a3*s3) / (a*a3)) ** oneeight

  lumd = 2.998e5/H0*(1.+z)*tmp*(eta1-eta2)

end function lumd


end module cosmology
