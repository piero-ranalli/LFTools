module startup
! initialize arrays, read cats, etc

use precision
use ml
use cosmology
use lumf_funcs

implicit none


type(lf_with_likelihood) :: like
!type(doublepowerlaw), target :: dp
!type(lddevol), target :: ldde
!type(ladevol), target :: lade

integer :: numparams
real(kind=rkind), allocatable :: scaledparams(:)

contains

  subroutine allocateLF(ev,zmin,zmax,lmin,lmax)
    character*(*) :: ev
    real(kind=rkind) :: zmin,zmax,lmin,lmax
    
    allocate(doublepowerlaw::like%z0func)

    if (ev .eq. 'lade') then
       allocate(ladevol::like%evol)
    else if (ev .eq. 'ladebpl') then
       allocate(ladevol_ueda::like%evol)
    else if (ev .eq. 'ldde') then
       allocate(lddevol::like%evol)
    else if (ev .eq. 'ldde15') then
       allocate(ldde15evol::like%evol)
    else if (ev .eq. 'pdle') then
       allocate(pdlevol::like%evol)
    else if (ev .eq. 'noevol') then
       allocate(noevol::like%evol)
    else
       write (*,*) 'Unrecognized evolution type.'
       stop
    end if

    like%zmin = zmin
    like%zmax = zmax
    like%lmin = lmin
    like%lmax = lmax
    
    call setcosmology(70.d0,.3d0,.7d0)
  end subroutine allocateLF

    

  
subroutine readcatalogue(i,catfile,areafile)
  integer :: i
  character*(*) :: catfile,areafile

  write (*,*) 'reading catalogue and area: ',trim(catfile),' ',trim(areafile)
  call like%cat(i)%readcat(catfile,like%zmin,like%zmax,like%lmin,like%lmax)
  call like%cat(i)%readarea(areafile)


end subroutine readcatalogue


subroutine setlastcat(i)
  integer :: i

  write (*,*) i,' catalogues read.'
  like%lastcat = i
  
  write (*,*) 'done.'
end subroutine setlastcat


subroutine start_umarginal
  write (*,*) 'setting up U marginal probabilities...'
  like%do_nhcorr = .true.
  call like%setup_umarginal
end subroutine start_umarginal


end module startup
