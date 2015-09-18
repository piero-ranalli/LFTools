module ml_interop

use ml
use cosmology
use, intrinsic :: ISO_C_BINDING


implicit none


type(lf_with_likelihood) :: like
type(doublepowerlaw), target :: dp
type(lddevol), target :: ldde
!type(ladevol), target :: lade

!integer,parameter :: numparams=8
integer,parameter :: numparams=9
!integer :: outunit

contains

subroutine readcatalogue() bind(C)

  like%z0func => dp
  like%evol => ldde
  !like%evol => lade

  call setcosmology(70.,.3,.7)

  !write (*,*) 'setting lock...'
  !call set_lock_or_die

  write (*,*) 'reading catalogue...'
  call like%cat%read('catalogue.dat')
  write (*,*) 'reading areas...'
  call like%cat%read_lss_cdfs_areas
  !write (*,*) 'opening info unit...'
  !open(newunit=outunit, file='xvals.dat', status='new')

  !call release_lock
  write (*,*) 'done.'

end subroutine readcatalogue



function calclikelihood( xval ) bind(C, name="likelihood")
  real(c_float) calclikelihood
  real(c_float) :: xval(*)
  integer i

    select type ( z0 => like%z0func )
       class is (doublepowerlaw)
          !    set(A,gamma1,gamma2,Lstar)
          call z0%set(real(xval(1)),real(xval(2)),real(xval(3)),real(xval(4)))
    end select

    select type ( ev => like%evol )
      class is (ladevol)
         !    set(zc, p1, p2, d)
         call ev%set(real(xval(5)),real(xval(6)),real(xval(7)),real(xval(8)))

      class is (lddevol)
         !    set(zc, p1, p2, alfa, La)
         call ev%set(real(xval(5)),real(xval(6)),real(xval(7)),real(xval(8)),real(xval(9)))
    end select


  !write (outunit,*) 'xvals:', (xval(i),i=1,numparams)

  calclikelihood = like%likelihood()

end function calclikelihood


function priorprob( xval ) bind(C)
  real(c_float) priorprob
  real(c_float) :: xval(numparams)
  real :: rmin(numparams),rmax(numparams),norm(numparams)
  logical, save :: has_norm = .false.
  real :: prob
  integer i

  ! here is were we calculate the (log) prior probability
  ! we have uniform probabilities on all of them, defined in bayes_lumf
  ! and already in log-scale
    select type ( z0 => like%z0func )
       class is (doublepowerlaw)
          !    set(A,gamma1,gamma2,Lstar)
          call z0%set(real(xval(1)),real(xval(2)),real(xval(3)),real(xval(4)))
    end select

    select type ( ev => like%evol )
      class is (ladevol)
         !    set(zc, p1, p2, d)
         call ev%set(real(xval(5)),real(xval(6)),real(xval(7)),real(xval(8)))

      class is (lddevol)
         !    set(zc, p1, p2, alfa, La)
         call ev%set(real(xval(5)),real(xval(6)),real(xval(7)),real(xval(8)),real(xval(9)))
    end select

  priorprob = like%z0func%priorprob() + like%evol%priorprob()
end function priorprob


end module ml_interop
