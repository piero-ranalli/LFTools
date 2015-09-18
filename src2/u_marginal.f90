module u_marginal

! marginal probability for U
!
! double-precision version for ML/Bayesian code

use precision
use binarysearch

implicit none

integer, parameter, private :: probzsize = 121 ! these must be the
integer, parameter, private :: probUsize = 40  ! same as in dataread 
                                              
real(kind=rkind), dimension(probUsize), private, target :: problogU
real(kind=rkind), dimension(probzsize), private         :: probz

real(kind=rkind), dimension(probUsize,probzsize), private, target :: Umarginal



contains

!   procedure :: usize          => catalogue_usize
!   procedure :: read_umarginal => catalogue_read_umarginal


  subroutine read_umarginal(Upoint,usize)
    real(kind=rkind), dimension(:), pointer :: Upoint
    integer usize
    integer u,i,j
    real foo


    open (newunit=u, file='Umarginal-burlon.dat', status='old')
    ! format of the above file:
    ! 1st line, logU (40 of them)
    ! 2nd-last lines, 121 vectors of probabilities:
    !         1st column: redshift
    !         2nd-41st col: P(U,z)

    read (u,*)  (problogU(i),i=1,probUsize)

    do j = 1, probzsize
       read (u,*) probz(j), (Umarginal(i,j), i=1, probUsize)
    end do

    close (u)

    Upoint => problogU
    usize = probUsize
  end subroutine read_umarginal



  subroutine point_to_umarginal(p,z)
    real(kind=rkind), dimension(:), pointer :: p
    real(kind=rkind) z

    integer iz
    iz = nearest( probz, z )
    if (iz.eq.0)  iz=1

    p => Umarginal(:,iz)
  end subroutine point_to_umarginal


end module u_marginal
