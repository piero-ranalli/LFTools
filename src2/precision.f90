module precision

  ! define read kind

! if you change this parameter, update also the interfaces in integrals.f90
  integer, parameter :: rkind = 8

end module precision

! usage:
!
! use precision
! real(kind=rkind) :: myvariable
