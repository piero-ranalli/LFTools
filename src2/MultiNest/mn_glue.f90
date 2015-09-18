module mn_glue
! multinest - ML glue layer


use precision
use startup
use lumf_funcs

implicit none



contains



subroutine calclikelihood(l)
  real(kind=rkind) :: l
  integer :: i
  
    select type ( z0 => like%z0func )
       class is (doublepowerlaw)
          !    set(A,gamma1,gamma2,Lstar)
          call z0%set(scaledparams(1),scaledparams(2),scaledparams(3),scaledparams(4))
    end select

    select type ( ev => like%evol )
      class is (ladevol)
         !    set(zc, p1, p2, d)
         call ev%set(scaledparams(5),scaledparams(6),scaledparams(7),scaledparams(8))

      class is (lddevol)
         !    set(zc, p1, p2, alfa, La)
         call ev%set(scaledparams(5),scaledparams(6),scaledparams(7),scaledparams(8),scaledparams(9))

      class is (ldde15evol)
         !    set(zc1, p1star, beta1, Lp, p2, alfa1, La1, zc2, p3, alfa2, La2)
         call ev%set(scaledparams(5),scaledparams(6),scaledparams(7),scaledparams(8), &
                     scaledparams(9),scaledparams(10),scaledparams(11),scaledparams(12),&
                     scaledparams(13),scaledparams(14),scaledparams(15))

    end select


  !write (*,*) 'scaledparams:', (scaledparams(i),i=1,numparams)

  l = -.5d0*like%likelihood()
  !write (*,*) l
  
end subroutine  calclikelihood

end module mn_glue
