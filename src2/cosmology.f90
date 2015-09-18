module cosmology

use precision

  real(kind=rkind), parameter :: c=299792.458d0
  real(kind=rkind) :: OM = .3
  real(kind=rkind) :: OL = .7
  real(kind=rkind) :: H0 = 70

contains

  subroutine setcosmology(hubble,matter,lambda)
    real(kind=rkind),intent(in) :: hubble,matter,lambda
    
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

pure elemental real(kind=rkind) function covol(z)
  implicit none
  real(kind=rkind), intent(in) :: z

!     covol=dV/dz (IL VOLUME COMPRESO TRA z E z+dz) !!!
!     lumd is in Mpc
!     covol in Mpc**3         

  covol = 2.998d5*lumd_uelipen(z)**2 /  &
       ( H0*(1.d0+z)**3 * sqrt(OM*(1.d0+z)+OL/(1.d0+z)**2 ) )

end function covol



impure elemental real(kind=rkind)  function lumd(redshift)
  implicit real*8 (a-z)
  intent(in) :: redshift

  integer k                 ! parametro di curvatura
  !external d_lum
      
  common/para/Omega0,lambda0,z,k0,k

  Omega0 = OM
  Lambda0 = OL
  z = redshift
      
  ! H0 OmegaM e OmegaL sono definiti all'inizio
  ! q0 e gli altri dovrebbero stare bene qui
  ! del flusso non mi frega

  cost2=2.d0*sqrt(2.d0*log(2.d0))

  q0=Omega0/2.d0-lambda0
  k0=Omega0+lambda0-1.d0

!     Calcolo dell'integrale della formula della distanza di luminosita' 
!     nell'intervallo di integrazione 1/(1+z) e 1 .
!     NON Si fa uso della subroutine QGAUSS del Numerical Recipes.
!     che mi sono rotto e uso solo cernlib da oggi in poi :( 
         
  min=1.d0/(1.d0+z)
  cost=c*(1.d0+z)/H0
  max=1.d0
  !dl_int = gauss(d_lum,min,max,1.e-3) 
  call qgaus(d_lum,min,max,dl_int) 
         

10 continue
         
  ! dl e' in Mpc
  if (k0.lt. 0) then
     dl=(cost/sqrt(-k0))*sinh(dl_int)
  else if (k0.eq. 0) then
     dl=cost*dl_int
  else if(k.gt. 0) then
     dl=cost/sqrt(k0)*sin(dl_int)
  end if

  lumd = dl
  return

end function lumd
      
real*8 function d_lum(y)
  implicit real*8  (a-z)
  integer k
  common/para/Omega0,lambda0,z,k0,k
  if (k0.lt. 0) then
     d_lum=sqrt(-k0)/(y*sqrt(Omega0/y-k0+lambda0*y**2))
  else if (k0.eq. 0) then
     d_lum=1.d0/(y*sqrt(Omega0/y+lambda0*y**2))
  else if(k0.gt. 0) then
     d_lum=sqrt(k0)/(y*sqrt(Omega0/y-k0+lambda0*y**2))
  end if
  return 
end function d_lum


SUBROUTINE qgaus(funct,a,b,ss)
  !  (C) Copr. 1986-92 Numerical Recipes Software ]2#1.[
  implicit real*8 (a-h,o-z)
  REAL(kind=rkind) :: a,b,ss,funct
  EXTERNAL funct
  INTEGER j
  REAL(kind=rkind) :: dx,xm,xr,w(5),x(5)
  SAVE w,x
  DATA w/.2955242247d0,.2692667193d0,.2190863625d0,.1494513491d0,.0666713443d0/
  DATA x/.1488743389d0,.4333953941d0,.6794095682d0,.8650633666d0,.9739065285d0/
  xm=0.5d0*(b+a)
  xr=0.5d0*(b-a)
  ss=0
  do j=1,5
     dx=xr*x(j)
     ss=ss+w(j)*(funct(xm+dx)+funct(xm-dx))
  end do
  ss=xr*ss
  return
END subroutine qgaus



pure elemental real(kind=rkind) function lumd_uelipen (z)
  implicit none
  real(kind=rkind), intent(in) :: z
  real(kind=rkind) :: s,s3,a,as,a3,eta1,eta2,tmp
  real(kind=rkind), parameter :: onethird = 1./3.
  real(kind=rkind), parameter :: oneeight = -1./8.
     
  real(kind=rkind) :: x,x3,eta
  eta(x,x3) = 1. +x*(-.1540 + x*.4304) +x3*(.19097 +x*.066941)
  
  s3  = ((1.-OM)/OM)

  s = s3**onethird
  tmp = 2.*sqrt(s3+1)
  eta1 = eta(s,s3) ** oneeight

  a=1./(1.+z)
  a3 = a**3
  eta2 = (eta(a*s,a3*s3) / (a*a3)) ** oneeight

  lumd_uelipen = 2.998e5/H0*(1.+z)*tmp*(eta1-eta2)

end function lumd_uelipen


end module cosmology
