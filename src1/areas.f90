module areas
use curves
use fileutils

! SYNOPSIS
!
! type(coverage) :: area,areacdfs
! call area%readlss
! call areacdfs%readcdfs
! call area%add(areacdfs)
! area_at_flux_f = area%interpolate(f)
!
! DESCRIPTION
!
! fluxes are in Log scale; areas in deg**2

implicit none

type, extends(curve) :: coverage
    character*(23), private :: areafile = 'area_hard.txt'
 contains
   procedure set_corrected_area
   procedure read
   procedure readlss
   procedure readcdfs
   procedure readcosmos
   procedure add
   procedure save
end type coverage

contains

  subroutine read (this,filename)
    class(coverage) :: this
    character(*), intent(in) :: filename
    integer :: u,i
    character*256 row

    ! clean arrays
    this%x = 0.
    this%y = 0.
    
    ! apre il file delle aree
    write (*,*) 'reading areas from file: ', filename
    open (newunit=u, file=filename, status='old')

    i = 1
    do
       read (u,'(A)',end=3)  row
    
       if (iscomment(row))  cycle

       read (row,*) this%x(i),this%y(i)

       i=i+1
    end do

3   close (unit=u)
    this%last = i-1
    
    ! put safeguard against extrapolation
    ! (areas curves should be considered constant for fluxes brighter
    !  than the last specified one)
    if (this%x( this%last ) < -5.) then
       this%last = this%last + 1
       this%x( this%last ) = -5.
       this%y( this%last ) = this%y( this%last-1 )
    end if


  end subroutine read


  
  subroutine set_corrected_area (this,corr)
    class(coverage) :: this
    logical corr

    if (corr) then
       !this%areafile = 'area_hard_corr.txt'
       this%areafile = 'corrected-lss-area2.dat'
    else
       this%areafile = 'area_hard.txt'
    endif
  end subroutine set_corrected_area


  subroutine readlss (this)
    class(coverage) :: this
    integer :: u,i
    character*3 foo


    ! apre il file delle aree
    write (*,*) 'reading lss areas from file: ',this%areafile
    open (newunit=u, file=this%areafile, status='old')

    ! salta le prime due righe che sono di commento
    read (u,1) foo
    read (u,1) foo
1   format (A)

    i = 1

2   read (u,*,end=3)  this%x(i),this%y(i)
    this%x(i) = log10(this%x(i))

    i=i+1
    goto 2

3   close (unit=u)
    this%last = i-1

  end subroutine readlss


  subroutine readcdfs (this)
    class(coverage) :: this
    integer :: u,i
    character*3 foo

    ! apre il file delle aree
    open (newunit=u, file='xmmcdfs-completeness210.dat', status='old')

    ! salta le prime due righe che sono di commento
    read (u,1) foo
    read (u,1) foo
1   format (A)

    i = 1

2   read (u,*,end=3)  this%x(i),this%y(i)

    i=i+1
    goto 2

3   close (unit=u)
    this%last = i-1

  end subroutine readcdfs


  subroutine readcosmos (this)
    class(coverage) :: this
    integer :: u,i
    character*3 foo

    ! apre il file delle aree
    open (newunit=u, file='xmmcosmos-area.dat', status='old')

    ! salta le prime due righe che sono di commento
    read (u,1) foo
    read (u,1) foo
1   format (A)

    i = 1

2   read (u,*,end=3)  this%x(i),this%y(i)

    i=i+1
    goto 2

3   close (unit=u)
    this%last = i-1

  end subroutine readcosmos


  subroutine add (this,that)
    class(coverage) :: this,that
    type(curve) :: tmp
    integer i
    real f

    ! arealss is nolog,nolog; areacdfs is log,nolog
    do i=1, this%last

       f = that%interpolate( this%x(i) )
       tmp%y(i) = this%y(i) + f

    end do

    this%y = tmp%y

  end subroutine add


subroutine save(this,filename)
  class(coverage) :: this
  character*(*)  filename
  integer u,i
  open (newunit=u, file=filename, status='new')

  do i=1,this%last
     write (u,*) 10.**this%x(i), this%y(i)
  enddo

  close(unit=u)

end subroutine save






end
