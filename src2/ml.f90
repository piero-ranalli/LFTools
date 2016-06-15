module ml

! maximum-likelihood stuff
use precision
use integrals
use lumf_funcs
use catalogueio
use cosmology
use u_marginal

implicit none


! SYNOPSYS
!
! type(lf_with_likelihood) :: dplike
! type(doublepowerlaw) :: dp
! type(lddevol) :: ldde
! dplike%z0func => dp
! dplike%evol => ldde
!
! dplike%cat%read(filename)
! dplike%z0func%set(A,gamma1,gamma2,Lstar)
! dplike%evol%set(zc, p1, p2, alfa, La)
! l = dplike%likelihood


! Choice between LADE and LDDE is done in SELECT TYPE statement inside doublepowerlaw%lfcalc
!
type, extends(luminosityfunction) :: lf_with_likelihood
   integer :: lastcat          ! number of catalogues
   type(catalogue), dimension(10) :: cat
   real(kind=rkind) :: zmin,zmax,lmin,lmax
   
   ! U marginal probabilities
   logical :: do_nhcorr = .false.
   integer :: usize
   real(kind=rkind), dimension(:), pointer :: problogU
 contains
   procedure :: setup_umarginal => catalogue_setup_umarginal
   procedure :: likelihood => all_catalogues_likelihood
   procedure :: onecat_likelihood => one_catalogue_likelihood
!   procedure :: integrandL,integrandz
end type lf_with_likelihood


integer, private ::  catno  ! pass the cat number to the contained functions



contains

subroutine catalogue_setup_umarginal(this)
  class(lf_with_likelihood) :: this
  call read_umarginal(this%problogU, this%usize)
end subroutine catalogue_setup_umarginal


function all_catalogues_likelihood(this)   result(S)
  class(lf_with_likelihood) :: this
  real(kind=rkind) :: S
  integer ic

  S = 0.
  do ic=1,this%lastcat
     S = S + this%onecat_likelihood(ic)
  end do
end function all_catalogues_likelihood


function one_catalogue_likelihood(this,ic)   result(S)
  use integrals
  class(lf_with_likelihood) :: this
  real(kind=rkind) :: S
  integer, intent(in) :: ic

  !real, parameter :: zmin = .001, zmax = 1.5, lmin=41., lmax=45.5
  ! these should match the catalogue!
  !real(kind=rkind), parameter :: zmin = .0001, zmax = 4., lmin=41., lmax=46.
  real(kind=rkind) :: lf,partialsum

  real(kind=rkind), allocatable :: weightedlf(:)  !(size(this%cat%Lx))

  integer i, id,unique, idstart,idstop

  S = 0.

  ! sum over all sources; and for each source, integrate over all
  ! possibilities for absorption correction and photo-z pdf

  catno = ic
  id = this%cat(ic)%id(1)
  partialsum = 0.

  idstart=1
  do i=1, this%cat(ic)%size  ! n=size of Lx,z
     ! last entry of a given id?
     if (i==this%cat(ic)%size .or. this%cat(ic)%id(i).ne.this%cat(ic)%id(i+1)) then
        idstop=i

        ! calculemus !
        allocate( weightedlf(idstop-idstart+1) )

        ! NB lfcalc's output is already in Log-scale
        weightedlf = 10.**this%lfcalc( this%cat(ic)%Lx(idstart:idstop), &
                                       this%cat(ic)%z(idstart:idstop)   &
                          ) * covol(this%cat(ic)%z(idstart:idstop))     &
                            * this%cat(ic)%weight(idstart:idstop)

        partialsum = sum(weightedlf)
        deallocate( weightedlf )

        ! avoid taking log(0)
        if (partialsum < 1.e-35)  partialsum=1.e-35

        ! account for inclusion probability ('match probability')
        ! for simplicity, we consider a single value for each source
        ! so we can just use includeprob(i)
        ! (lf-binned is slightly more general and in principle allows different
        !  includeprob for the same source, say if you have a parameter-dependent
        !  includeprob)
        ! NB this modification to the likelihood is not in Ranalli et al. 2016
        ! but will likely appear in Akylas et al. 2016
        S = S - this%cat(ic)%includeprob(i) * log( partialsum )

        partialsum = 0.

        idstart=i+1
     endif
  enddo


  ! integral over L  (over z is done inside integrandz)
  ! (as in the code for Page-Carrera method: pageca_dev.f / lumfunc.f)
  S = S + integrate(integrandL,this%lmin,this%lmax)/3283.d0
  !S = S + integrate_cuhre_2d(integrandz,zmin,zmax,lmin,lmax)/3283.d0

  ! multiply by 2 so that uncertainty estimates are in the same scale of those
  ! obtained with the chi**2
  S = 2. * S

   

contains

  ! using internal procedures, which access all variables via host
  ! association. But since they cannot be nested, and integrandL
  ! requires two parameter, I had to define another integration
  ! procedure accepting the additional param in the integrals module.

  real(kind=rkind) function integrandL(Lx)
    real(kind=rkind),intent(in) :: Lx
    !real, external :: integrandL

    integrandL = integrate(integrandz,this%zmin,this%zmax,Lx)

  end function integrandL


  real(kind=rkind) function integrandz(z,Lx)
    ! compare with zinte() and covolf() in Page-Carrera program's lumfunc.f

    real(kind=rkind), intent(in) :: z,Lx
    real(kind=rkind) :: finteg   ! flux correspondig to Lx placed at z
    real(kind=rkind) :: area,tmp ! coverage at finteg, normalized to full_sky=1
    integer :: i
    real(kind=rkind), dimension(:), pointer :: umarg

                 !  log10(4*pi)    log10(3.086e24**2)
    finteg = Lx -1.09920977366969d0 -48.978791843454259d0 - 2.*log10( lumd(z) )

    if (this%do_nhcorr) then
       ! average area over all possible observed fluxes
       call point_to_umarginal( umarg, z )
       area = 0.d0
       do i=1, this%usize
          tmp = this%cat(catno)%coverage(finteg - this%problogU(i))
          area = area + umarg(i) * tmp
       end do
    else
       area = this%cat(catno)%coverage(finteg)
    end if

    !area = area/3283.d0 ! this can be safely moved out of the integration loop

    if (area<0.)   area=0.   ! don't extrapolate too much..

    integrandz = 10.**this%lfcalc(Lx,z) * area * covol(z)

    if (integrandz<0)  then
       write (*,*) '???? integrandz<0'
       write (*,*) 'finteg=',finteg,'area=',area,'integrandz=',integrandz
    endif
  end function integrandz


end function one_catalogue_likelihood



end module ml
