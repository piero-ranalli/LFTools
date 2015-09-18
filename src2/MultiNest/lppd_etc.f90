module lppd_etc

! SYNOPSIS
!
! type(tlppd) :: lppd
! call lppd%new( ncvcats )   ! set number of cross-validation (CV) cats
! call lppd%setallsurveys( pew ) ! set filename for all-surveys 
!                                ! post-equal-weights (pew)
! waic = lppd%waic      ! calc the watanabe-akaike information criterion
! call lppd%deallocate  ! de-allocate structures before loading a new pew

! private methods:
! lppd_cati  = lppd%single_lppd( i )  ! calc the lppd for cat #i
! pwaic_cati = lppd%single_pwaic2( i ) ! calc pwaic2 for cat #i 

  use precision
  use fileutils
  use startup
  use params
  use priors
  
  implicit none
  
  integer, parameter, private :: n_likes = 10
  
  type :: tlppd
     integer :: ncvcats, nsamples
     real(kind=rkind), dimension(:,:), allocatable :: pew_allsurv
     real(kind=rkind), dimension(:),   allocatable :: pew_loglike,datalikelihood,dataloglikelihood
     integer :: pew_loglike_maxidx  ! index of maximum-likelihood record in pew_loglike
   contains
     procedure :: new           => lppd_new
     procedure :: setallsurveys => lppd_setallsurveys
     procedure :: aic           => lppd_calc_aic
     procedure :: waic          => lppd_calc_waic
     procedure :: deallocate    => lppd_deallocate
!     procedure :: readcv        => lppd_readcv
!     procedure :: calc_cv       => lppd_calc_waic

     ! "private" methods (i.e. less interesting so unlikely to be called
     !                    from outside)
     procedure :: single_lppd    => lppd_calc_single_lppd
     procedure :: single_pwaic2  => lppd_calc_single_pwaic2
     procedure :: set_pars_from_pew => lppd_set_pars_from_pew
  end type tlppd

  ! cannot use an allocatable array of likelihoods, because the like type already
  ! contains allocatable arrays inside. So let's put a limit of 10 surveys for the moment.
 ! these variables are not used at the moment:
!  type(lf_with_likelihood), private :: like_allsurv, likecv(n_likes)
  
  
contains
  
!> set number of cross-validation (CV) cats
  subroutine lppd_new( this, ncvcats )
    class(tlppd) :: this
    integer :: ncvcats

    if (ncvcats>n_likes) then
       write (*,*) 'Currently, this program has a limit of 10 items for cross-validation.'
       write (*,*) 'The limit can be overcome by changing the n_likes parameter in lppd.f90'
       write (*,*) 'and recompiling.'
       stop
    end if
    
    this%ncvcats = ncvcats
  end subroutine lppd_new


!> read the all-surveys post-equal-weights (pew) file, scale and store
!! posterior samples
!! also allocate the datalikelihood array
  subroutine lppd_setallsurveys( this, pew, maxsamples )
    class(tlppd) :: this
!    class(lf_with_likelihood) :: like
    character(len=256) :: pew
    integer :: i,j,u,foo
    integer, optional :: maxsamples
    real(kind=rkind), allocatable :: pewtmp(:)
    
!    this%like_allsurv = like


    this%nsamples = lines_in_file( pew )
    if (present(maxsamples) .and. this%nsamples>maxsamples) then
       this%nsamples=maxsamples
    end if
       
    allocate( this%pew_allsurv( numparams, this%nsamples ) )
    allocate( this%pew_loglike(this%nsamples) )
    allocate( pewtmp(numparams) )
    open( newunit=u, file=pew, status='old' )
    do i=1,this%nsamples
       read (u,*) ( pewtmp(j), j=1, numparams ), this%pew_loglike(i)

       ! scale params. pewtmp has the role of multinest's cube, so
       ! we can use the same mechanism we use in mn_like.f90
       ! This will automatically take care of flat/cauchy/gamma parameters etc
       call prior%scalepar( pewtmp, this%pew_allsurv(:,i) )
    end do
    deallocate(pewtmp)

    this%pew_loglike_maxidx = maxloc(this%pew_loglike,1)

    ! allocate datalikelihood
    allocate( this%datalikelihood(this%nsamples),    &
              this%dataloglikelihood(this%nsamples) )
  end subroutine lppd_setallsurveys


!> deallocate this%pew_allsurv and this%datalikelihood
!! (to be called before loading a new pew)
  subroutine lppd_deallocate( this )
    class(tlppd) :: this

    deallocate( this%pew_allsurv,       this%pew_loglike,     &
                this%dataloglikelihood, this%datalikelihood )
  end subroutine lppd_deallocate



!> computed lppd for all surveys (Eq.7.5 in BDA3)
  function lppd_calc_waic( this )  result (waic)
    class(tlppd) :: this
    real(kind=rkind) :: waic
    real(kind=rkind) :: l,p
    integer :: i

    l = 0.
    p = 0.
    !write (*,*) 'Starting WAIC computing'

    do i=1, like%lastcat
       l = l+this%single_lppd(i)
       !write (*,*) 'lppd(',i,') = ',l
       p = p+this%single_pwaic2()
       !write (*,*) 'pwaic2(',i,') = ',p
    end do
    waic = -2*(l-p)
  end function lppd_calc_waic

!> computed lppd for a single survey (Eq.7.5 in BDA3 without the sum over n)
  function lppd_calc_single_lppd( this, catidx )  result (l)
    class(tlppd) :: this
    integer, intent(in) :: catidx
    real(kind=rkind) :: l
    integer :: i
    real(kind=rkind) :: minl

    do i=1,this%nsamples
       !write (*,*) i

       call this%set_pars_from_pew(i)

       ! like% is given by the startup module
       this%dataloglikelihood(i) = -.5d0*like%onecat_likelihood(catidx)
    end do

    ! datalikelihood is in log-scale; move to linear scale to obtain
    ! p(y_i|theta_s), of which the average should be taken
    !write (*,*) this%dataloglikelihood

    ! subtract the minimum before taking exp to avoid overflows to infinity
    minl = minval(this%dataloglikelihood)
    this%datalikelihood = exp(this%dataloglikelihood - minl)
    
    !write (*,*) this%datalikelihood
    ! readd the minimum to the log(average)
    l = minl+log( sum(this%datalikelihood) / this%nsamples )

  end function lppd_calc_single_lppd






!> computed pwaic2 for a single survey (Eq.7.12 in BDA3 without the sum over n)
!!  1/(S-1) \sum_s (ln P(y|theta_s) - <ln P(y|theta_s)>)^2
!! using the datalikelihood precomputed by lppd_calc_singlelppd
!! (as it uses the datalikelihood array, it can only be called after 
!!  %single_lppd)
  function lppd_calc_single_pwaic2( this )  result (pwaic2)
    class(tlppd) :: this
    real(kind=rkind) :: pwaic2, logavg, variance
    real(kind=rkind), dimension(size(this%datalikelihood)) :: logdatalike

    logavg = sum( this%dataloglikelihood ) / this%nsamples
    variance = sum( (this%dataloglikelihood - logavg)**2 )

    pwaic2 = variance / (this%nsamples-1)
  end function lppd_calc_single_pwaic2



!> AIC (BDA3 page 172)
  function lppd_calc_aic( this )  result (aic)
    class(tlppd) :: this
    real(kind=rkind) :: aic

    call this%set_pars_from_pew( this%pew_loglike_maxidx )
    !aic = -2 * (-.5d0*like%likelihood() - k)
    aic = like%likelihood() + 2*numparams
  end function lppd_calc_aic
  

subroutine lppd_set_pars_from_pew( this,i )
    class(tlppd) :: this
    integer, intent(in) :: i
    
    select type ( z0 => like%z0func )
       class is (doublepowerlaw)
          !    set(A,gamma1,gamma2,Lstar)
       call z0%set(                                       &
            this%pew_allsurv(1,i),this%pew_allsurv(2,i),  &
            this%pew_allsurv(3,i),this%pew_allsurv(4,i)   &
            )
    end select

    select type ( ev => like%evol )
       class is (ladevol)
          !    set(zc, p1, p2, d)
       call ev%set(                                       &
            this%pew_allsurv(5,i),this%pew_allsurv(6,i),  &
            this%pew_allsurv(7,i),this%pew_allsurv(8,i)   &
            )

       class is (lddevol)
          !    set(zc, p1, p2, alpha, La)
       call ev%set(                                       &
            this%pew_allsurv(5,i),this%pew_allsurv(6,i),  &
            this%pew_allsurv(7,i),this%pew_allsurv(8,i),  &
            this%pew_allsurv(9,i) ) 

       class is (ldde15evol)
          !    set(zc, p1, p2, alpha, La)
       call ev%set(                                         &
            this%pew_allsurv( 5,i),this%pew_allsurv( 6,i),  &
            this%pew_allsurv( 7,i),this%pew_allsurv( 8,i),  &
            this%pew_allsurv( 9,i),this%pew_allsurv(10,i),  &
            this%pew_allsurv(11,i),this%pew_allsurv(12,i),  &
            this%pew_allsurv(13,i),this%pew_allsurv(14,i),  &
            this%pew_allsurv(15,i)  ) 
    end select
  end subroutine lppd_set_pars_from_pew
  
end module lppd_etc
