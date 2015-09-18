module mn_like

use precision
use mn_glue	
use params

implicit none
      
contains
      
      
!=======================================================================

subroutine slikelihood(Cube,slhood)
         
	implicit none
      
	double precision Cube(nest_nPar),slhood
        integer i
         
	!rescaling the parameters in unit hypercube according to the prior
        call prior%scalepar( Cube, scaledparams )

	! do i=1,sdim
	! 	scaledparams(i)=(spriorran(i,2)-spriorran(i,1))*Cube(i)+spriorran(i,1)
	! end do

        call calclikelihood(slhood)

end subroutine slikelihood
      
!=======================================================================

end module mn_like
