module gehrels

  real,private :: glow(0:50),ghigh(0:50)

contains

  subroutine gehrelsinit
    integer myunit
    character*3 foo
    real bar
1   format (A)

    ! this files contains the 1-sigma errors
    open(newunit=myunit,file='gehrels.dat',status='old')  !newunit=  is a Fortran2008 construct!
    read (myunit,1) foo
    read (myunit,1) foo
    do i=0,50
       read (myunit,*) bar,glow(i),ghigh(i)
    enddo
    close (unit=myunit)
      
  end subroutine gehrelsinit


  real function gehrelsl(i)
    integer i

    if (i<=50) then
       gehrelsl = glow(i)
    else
       gehrelsl = real(i)-sqrt(real(i))
    endif
  end function gehrelsl


  real function gehrelsh(i)
    integer i

    if (i<=50) then
       gehrelsh = ghigh(i)
    else
       gehrelsh = real(i)+sqrt(real(i))
    endif
  end function gehrelsh

end module gehrels
