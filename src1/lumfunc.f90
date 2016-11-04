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

    use cosmology

    implicit none

    real logl

    real intez
    real passo,z0

    !      f = flusso   ! perche' deve passarla a covol

    intez= 0
    passo = (zmax-zmin)/300.d0
    z0 = zmin
    do while (z0.le.zmax)

       !     prende il flusso a cui vedrebbe l'oggetto se fosse allo z di integrazione

       finteg = 10.**(logl-1.09920977366969d0-0.978791843454259d0-48.d0) / lumd(z0)**2.
       intez= intez+ covolf(z0)*passo
       z0 = z0 + passo

    enddo

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

    !     e ora il volume comovente (PR 3.7.03)
    !     n.b. lumd e' in Mpc

    !     QUESTO E' d/dz (IL VOLUME COMPRESO TRA z E z+dz) !!!


    dvdz = covol(redshift)

    flusso = finteg
    ! 10.d0 ** (linteg-2.0780016d0-48.-2.d0*log10(lumd(z)))
    !        la correzione K:  (Gamma=2.1)  stavolta e' (1+z)**(alfa-1)
    !        perche' voglio il flusso osservato a partire da quello emesso
    !         flusso = flusso * (1.d0+z)**(gamma-2.d0)



    !if (flusso.le.9.e-16) then
    !   write (*,*) 'flux error'
    !endif


    logflusso = log10(flusso)

    dvdz = dvdz * calccoverage(logflusso,redshift)

    covolf = dvdz

    return
  end function covolf



  real function simplecoverage(logf,z) ! simple coverage with no nhcorr
    real,intent(in) :: logf,z
    real area,tmp

    area = areapointer%interpolate(logf)
    simplecoverage = area/3283.
  end function simplecoverage


  

  real function obscoverage(logf,z) ! coverage at observed flux in sterad
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
          
       !write (*,*) z, problogU(i), umarg(i), tmp, area
    end do


    !write (*,*) 'unabs_area=',areapointer%interpolate(logf)
    !stop

    obscoverage = area / 3283.


    ! tmparea = areapointer%interpolate(logflusso) / 3823.

  end function obscoverage





end module lumfunc
