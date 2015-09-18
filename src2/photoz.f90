module photoz

! SYNOPSYS
!
! type(photoz_pdf) :: geneva
! geneva%readpdf(genevaid)
! zarray = pdf%z
! pdfarray = pdf%pdf

use precision

implicit none

integer, parameter, private :: skiplss = 63
integer, parameter, private :: skipcdfs = 0

integer, parameter, private :: ncolslss = 601
integer, parameter, private :: ncolscdfs = 606


character*(*), parameter, private :: pathlss  = '/Users/piero/Dati/Teoria/XLF2013/XMMLSS/SPEC_files/'
character*(*), parameter, private :: pathcdfs = '/Users/piero/Dati/Teoria/XLF2013/XMMLSS/cdfs-photoz/'


type photoz_pdf
   integer :: lastcol, skip, ncols
   character (len=100) :: path
   real(kind=rkind), dimension(:), allocatable :: z, pdf

 contains

   procedure :: chooselss => photoz_chooselss
   procedure :: choosecdfs => photoz_choosecdfs
   procedure :: readpdf => photoz_read_pdf

end type photoz_pdf

contains


  subroutine photoz_chooselss(this)
    class(photoz_pdf) :: this

    this%skip = skiplss
    this%ncols = ncolslss
    this%path = pathlss

    allocate (this%z(this%ncols))
    allocate (this%pdf(this%ncols))
  end subroutine photoz_chooselss

  subroutine photoz_choosecdfs(this)
    class(photoz_pdf) :: this

    this%skip = skipcdfs
    this%ncols = ncolscdfs
    this%path = pathcdfs

    allocate (this%z(this%ncols))
    allocate (this%pdf(this%ncols))
  end subroutine photoz_choosecdfs


  subroutine photoz_read_pdf(this,id)
    class(photoz_pdf) :: this
    integer, intent(in) :: id
    integer u,i
    real(kind=rkind) :: ss

    character(9) :: cid,foo


1   format (I9.9)
    write (cid,1) id

    open (newunit=u, file=trim(this%path)//'Id'//cid//'.spec', status='old')

    do i=1,this%skip
       read (u,*) foo
    end do

    do i=1,this%ncols
       read (u,*) this%z(i), this%pdf(i)
    end do

    close(u)

    ss = sum(this%pdf)
    if (abs(ss - 1.) > .01) then
        !print *,'PDF not normalized for source opticalid=',id,'  sum=',ss,'delta=',abs(ss-1.)
       ! renorm... (O. Ilbert said renorm is ok)
       this%pdf = this%pdf / ss
    else
       !print *,'PDF is normalized!'
    end if

    this%lastcol = this%ncols
  end subroutine photoz_read_pdf

end module photoz
