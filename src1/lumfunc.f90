module lumfunc

use areas
use u_marginal


real :: llmin,llmax,zmin,zmax
type(coverage), pointer :: areapointer
real, private :: finteg  ! flux for integration
!integer :: probUsize

integer :: usize
real, dimension(:), pointer :: problogU

abstract interface
   function icoverage(logf,z)
     real :: icoverage
     real,intent(in) :: logf,z
   end function icoverage
end interface

procedure (icoverage), pointer :: calccoverage => null()



contains

  subroutine lumfunc_setup(Umarg)
    character(*) :: Umarg
    call read_umarginal(Umarg, problogU, usize)
  end subroutine lumfunc_setup


  real function zinte(logl)

    ! this routine computes the inner integral (on dz)
    ! in Eq.9 in Ranalli et al. 2016

    use cosmology

    implicit none

    real logl

    real intez
    real passo,z0   ! passo == step

    ! cycle on dz
    intez= 0
    passo = (zmax-zmin)/300.d0
    z0 = zmin
    do while (z0.le.zmax)

       ! finteg is the flux that an object would have if it was placed
       ! at the running z
       ! NB finteg is a module-scoped variable, used in covolf
       !    this arrangement is such that a library function for integrals
       !    could be used (e.g., qgaus or dgauss); though in the end it seems
       !    better to do a simple cycle
       finteg = 10.**(logl-1.09920977366969d0-0.978791843454259d0-48.d0) / lumd(z0)**2.
       intez= intez+ covolf(z0)*passo
       z0 = z0 + passo

    enddo

    ! (alternatively, the integral could be done with one of the following calls
    !      call qgaus (covolf,zmin,zzz,intez) ! NB zeta e non zmax
    !      intez = dgauss(covol,zmin,zmax,1.d-4)
    !      call dqdags (covol,zmin,zmax,0.d0,1.d-4,intez,intezerr)

    zinte = intez

  end function zinte






  real function covolf(redshift)   ! comoving volume times coverage

    use cosmology  ! also makes OM,OL,H0 availale

    implicit none

    real redshift,dvdz,z
    real flusso,logflusso

    integer i

    ! dV/dz i.e. volume between z and z+dz
    dvdz = covol(redshift)

    ! inverse K correction
    flusso = finteg
    ! 10.d0 ** (linteg-2.0780016d0-48.-2.d0*log10(lumd(z)))
    !        la correzione K:  (Gamma=2.1)  stavolta e' (1+z)**(alfa-1)
    !        perche' voglio il flusso osservato a partire da quello emesso
    !         flusso = flusso * (1.d0+z)**(gamma-2.d0)



    !if (flusso.le.9.e-16) then
    !   write (*,*) 'flux error'
    !endif


    logflusso = log10(flusso)

    ! calccoverage is a pointer to either simplecoverage or observedcoverage
    dvdz = dvdz * calccoverage(logflusso,redshift)

    covolf = dvdz

    return
  end function covolf



  real function simplecoverage(logf,z)
    ! simple coverage with no nhcorr
    ! implementing Eq.6 in Ranalli et al. 2016
    real,intent(in) :: logf,z
    real area,tmp

    area = areapointer%interpolate(logf)
    simplecoverage = area/3283.
  end function simplecoverage


  

  real function obscoverage(logf,z)
    ! coverage at observed flux in sterad
    ! implementing Eq.9 in Ranalli et al. 2016
    ! (observed means: absorbed. Hence, average it on P(U)
    real,intent(in) :: logf,z

    real, dimension(:), pointer :: umarg
    real area,tmp
    integer i

    call point_to_umarginal( umarg, z )
    area = 0.
    do i=1, usize
       tmp = areapointer%interpolate(logf - problogU(i))
       area = area + umarg(i) * tmp
    end do

    obscoverage = area / 3283.

  end function obscoverage



end module lumfunc
