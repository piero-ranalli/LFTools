program maximumlikelihood

use ml
use cosmology

type(lf_with_likelihood) :: like
type(doublepowerlaw), target :: dp
type(lddevol), target :: ldde
!type(ladevol), target :: lade

real l
integer i

call setcosmology(70.,.3,.7)

  like%z0func => dp
  like%evol => ldde
  !like%evol => lade


write (*,*) 'reading catalogue...'
call like%cat%read('catalogue.dat')
write (*,*) 'reading areas...'
call like%cat%read_lss_cdfs_areas
write (*,*) 'done.'

!    like%dpow%set(A,gamma1,gamma2,Lstar)
!call like%dpow%set(5.04,.86,2.23,43.94)
!    like%ldde%set(zc, p1, p2, alfa, La)
!call like%ldde%set(1.9, 4.23, -1.5, 0.335, 44.6)

! lafranca:
    select type ( z0 => like%z0func )
       class is (doublepowerlaw)
          !    set(A,gamma1,gamma2,Lstar)
          call z0%set(1.21,1.01,2.38,44.25)
    end select

    select type ( ev => like%evol )
      class is (ladevol)
         !    set(zc, p1, p2, d)
         call ev%set(2.49, 4.62, -1.15, 0.20)

      class is (lddevol)
         !    set(zc, p1, p2, alfa, La)
         call ev%set(2.49, 4.62, -1.15, 0.20, 45.74)
    end select



do i=1,10
   l = like%likelihood()
   write (*,*) l
end do

stop

l=42.
do while (l<47)
   write (*,*) l,like%lfcalc( l, 3.5 )
   l = l+.5
enddo


end program maximumlikelihood

