program maximumlikelihood

use lfmlconfig
use precision
use startup
use cosmology


use lumf_funcs

implicit none

!type(lf_with_likelihood) :: like
!type(doublepowerlaw), target :: dp
!type(lddevol), target :: ldde
!type(ladevol), target :: lade

!like%z0func => dp
!like%evol => ldde
!like%evol => lade
!allocate(doublepowerlaw::like%z0func)
!allocate(lddevol::like%evol)

call setcosmology(70.d0,.3d0,.7d0)

! write (*,*) 'reading catalogue...'
! !call like%cat%read('catalogue.dat')
! call like%cat(1)%readcat('catalogue-cdfs.dat')
! call like%cat(2)%readcat('catalogue.lss.dat')
! write (*,*) 'reading areas...'
! !call like%cat%read_lss_cdfs_areas
! call like%cat(1)%read_only_cdfs_area
! call like%cat(2)%read_only_lss_area
! like%lastcat=2

call configure  ! parse config file, allocate arrays, read catalogues etc


!write (*,*) 'setting up U marginal probabilities...'
!call like%setup_umarginal


write (*,*) 'starting minuit...'

!open (unit=19, file='minuit.dat.lade', status='old')
open (unit=19, file=minuitcmd, status='old')

call mintio(19,6,7)
call minuit(FCN,0.)

close(unit=19)


contains

  subroutine fcn (npar,grad,fval,xval,iflag,futil)
    integer :: npar,iflag
    ! the dimension for xval(*) is set by Minuit
    real (kind=rkind) :: grad(*),fval,xval(*),futil
    integer i

    select type ( z0 => like%z0func )
       class is (doublepowerlaw)
          !    set(A,gamma1,gamma2,Lstar)
          call z0%set(xval(1),xval(2),xval(3),xval(4))
    end select

    select type ( ev => like%evol )
      class is (ladevol)
         !    set(zc, p1, p2, d)
         call ev%set(xval(5),xval(6),xval(7),xval(8))

      class is (lddevol)
         !    set(zc, p1, p2, alfa, La)
         call ev%set(xval(5),xval(6),xval(7),xval(8),xval(9))

      class is (ldde15evol)
         !    set(zc1, p1star, beta1, Lp, p2, alfa1, La1, zc2, p3, alfa2, La2)
         call ev%set(scaledparams(5),scaledparams(6),scaledparams(7),scaledparams(8), &
                     scaledparams(9),scaledparams(10),scaledparams(11),scaledparams(12),&
                     scaledparams(13),scaledparams(14),scaledparams(15))
    end select

    !write (*,*) 'xvals:', (xval(i),i=1,9)

    fval = like%likelihood()

    if (iflag==1) then
       write (*,*) 'initial likelihood: ',fval
    elseif (iflag==3) then
       write (*,*) 'final likelihood: ',fval
    endif

  end subroutine fcn

end program maximumlikelihood

