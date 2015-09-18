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

  subroutine allocateLF(ev)
    character*(*) :: ev
    
    allocate(doublepowerlaw::like%z0func)

    if (ev .eq. 'lade') then
       allocate(ladevol::like%evol)
    else if (ev .eq. 'ladebpl') then
       allocate(ladevol_ueda::like%evol)
    else if (ev .eq. 'ldde') then
       allocate(lddevol::like%evol)
    else if (ev .eq. 'ldde15') then
       allocate(ldde15evol::like%evol)
    else
       write (*,*) 'Unrecognized evolution type.'
       stop
    end if
    
    call setcosmology(70.d0,.3d0,.7d0)
  end subroutine allocateLF

    

  
  subroutine readcatalogue(i,catfile,areafile)
    integer :: i
    character*(*) :: catfile,areafile

  !like%z0func => dp
  !like%evol => ldde
  !like%evol => lade

  !write (*,*) 'setting lock...'
  !call set_lock_or_die

  write (*,*) 'reading catalogue and area: ',trim(catfile),' ',trim(areafile)
  call like%cat(i)%readcat(catfile)
  call like%cat(i)%readarea(areafile)


end subroutine readcatalogue


subroutine setlastcat(i)
  integer :: i

  write (*,*) i,' catalogues read.'
  like%lastcat = i
  
  write (*,*) 'setting up U marginal probabilities...'
  call like%setup_umarginal

  !call release_lock
  write (*,*) 'done.'
end subroutine setlastcat



end module startup
