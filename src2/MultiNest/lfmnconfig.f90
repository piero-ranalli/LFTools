module lfmnconfig  ! configuration for lf-mn

  use startup
  use fson
  use fson_value_m, only: fson_value_count, fson_value_get
  use params
  
  implicit none
  integer, private :: i, ncats, nprioritems
  character(len=256), private :: config,cat,area,Umarginaldistr
  type(fson_value), pointer, private :: pars,catarray,catitem
  character(len=6), private :: evtype
  character(len=11), private :: priortype
  character(len=15), private :: keyword
  real, allocatable, private :: priorarray(:)
  real(kind=rkind), private  :: zmin,zmax,lmin,lmax
  logical, private           :: do_nhcorr = .false.
  
contains

  subroutine configure

    ! get parameters from configuration file
    call get_command_argument(1,config)
    if (len_trim(config)==0) then
       write (*,*) 'Please specify a configuration file.'
       stop
    end if

    pars => fson_parse(config)

    call fson_get(pars,"seed",nest_rseed)
    call fson_get(pars,"root",nest_root)
    call fson_get(pars,"evolution",evtype)
    call fson_get(pars,"priors",priortype)
    call fson_get(pars,"catalogues",catarray)
    call fson_get(pars,"nhcorr",do_nhcorr)
    call fson_get(pars,"limits.zmin",zmin)
    call fson_get(pars,"limits.zmax",zmax)
    call fson_get(pars,"limits.lmin",lmin)
    call fson_get(pars,"limits.lmax",lmax)

    ! allocate structures
    call allocatelf(evtype,zmin,zmax,lmin,lmax)
    if (do_nhcorr)  then
       call fson_get(pars,"Umarginal",Umarginaldistr)
       call start_umarginal(Umarginaldistr)
    end if

    
    ! read catalogues
    ncats = fson_value_count(catarray)
    do i=1, ncats
       catitem => fson_value_get(catarray, i)
       call fson_get(catitem,"cat",cat)
       call fson_get(catitem,"area",area)

       call readcatalogue(i,cat,area)
    end do

    call setlastcat(ncats)

    !setting prior type
    if (trim(priortype) .eq. 'flat') then
       allocate(flatprior::prior)
       nprioritems=2
       write (*,*) 'Using flat priors.'
    else if (priortype .eq. 'cauchygamma') then
       allocate(cauchygammaprior::prior)
       nprioritems=3
       write (*,*) 'Using Cauchy/Gamma priors.'
    else
       write (*,*) 'Unrecognized form for priors (use: "flat" or "cauchygamma").'
       write (*,*) 'You used: "',trim(priortype),'"'
       stop
    end if

    ! set prior parameters
    if (evtype .eq. 'noevol') then

       numparams = 4
       allocate (spriorpar(numparams,nprioritems))
       prior%params => spriorpar
       call setnoevolparams

    else if (evtype .eq. 'pdle') then

       numparams = 6
       allocate (spriorpar(numparams,nprioritems))
       prior%params => spriorpar
       call setpdleparams

    else if (evtype .eq. 'ldde') then

       numparams = 9
       allocate (spriorpar(numparams,nprioritems))
       prior%params => spriorpar
       call setlddeparams

    else if (evtype .eq. 'ldde15') then

       numparams = 15
       allocate (spriorpar(numparams,nprioritems))
       prior%params => spriorpar
       call setldde15params

    else if (evtype .eq. 'lade' .or. evtype .eq. 'ladebpl') then

       numparams = 8
       allocate (spriorpar(numparams,nprioritems))
       prior%params => spriorpar
       call setladeparams

    else
       write (*,*) 'Unrecognized evolution type (use: "pdle","ldde", "ldde15", "lade", or "ladebpl").'
       write (*,*) 'You used: "',evtype,'"'
       stop
    end if


  end subroutine configure



  subroutine setnoevolparams
    if (trim(priortype) .eq. 'flat') then
       keyword = "noevollimits"
    else
       keyword = "noevolcauchygamma"
    end if

    call fson_get(pars,trim(keyword)//".A",priorarray)
    call checkn
    spriorpar(1,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma1",priorarray)
    call checkn
    spriorpar(2,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma2",priorarray)
    call checkn
    spriorpar(3,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".Lstar",priorarray)
    call checkn
    spriorpar(4,:) = priorarray(:)
  end subroutine setnoevolparams



    subroutine setpdleparams
    if (trim(priortype) .eq. 'flat') then
       keyword = "pdlelimits"
    else
       keyword = "pdlecauchygamma"
    end if

    call fson_get(pars,trim(keyword)//".A",priorarray)
    call checkn
    spriorpar(1,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma1",priorarray)
    call checkn
    spriorpar(2,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma2",priorarray)
    call checkn
    spriorpar(3,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".Lstar",priorarray)
    call checkn
    spriorpar(4,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".etad",priorarray)
    call checkn
    spriorpar(5,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".etal",priorarray)
    call checkn
    spriorpar(6,:) = priorarray(:)
  end subroutine setpdleparams



  subroutine setlddeparams
    if (trim(priortype) .eq. 'flat') then
       keyword = "lddelimits"
    else
       keyword = "lddecauchygamma"
    end if

    call fson_get(pars,trim(keyword)//".A",priorarray)
    call checkn
    spriorpar(1,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma1",priorarray)
    call checkn
    spriorpar(2,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma2",priorarray)
    call checkn
    spriorpar(3,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".Lstar",priorarray)
    call checkn
    spriorpar(4,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".zc",priorarray)
    call checkn
    spriorpar(5,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".p1",priorarray)
    call checkn
    spriorpar(6,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".p2",priorarray)
    call checkn
    spriorpar(7,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".alpha",priorarray)
    call checkn
    spriorpar(8,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".La",priorarray)
    call checkn
    spriorpar(9,:) = priorarray(:)
  end subroutine setlddeparams



  subroutine setldde15params
    if (trim(priortype) .eq. 'flat') then
       keyword = "ldde15limits"
    else
       keyword = "ldde15cauchygamma"
    end if

    call fson_get(pars,trim(keyword)//".A",priorarray)
    call checkn
    spriorpar(1,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma1",priorarray)
    call checkn
    spriorpar(2,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma2",priorarray)
    call checkn
    spriorpar(3,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".Lstar",priorarray)
    call checkn
    spriorpar(4,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".zc1",priorarray)
    call checkn
    spriorpar(5,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".p1star",priorarray)
    call checkn
    spriorpar(6,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".beta1",priorarray)
    call checkn
    spriorpar(7,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".Lp",priorarray)
    call checkn
    spriorpar(8,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".p2",priorarray)
    call checkn
    spriorpar(9,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".alpha1",priorarray)
    call checkn
    spriorpar(10,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".La1",priorarray)
    call checkn
    spriorpar(11,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".zc2",priorarray)
    call checkn
    spriorpar(12,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".p3",priorarray)
    call checkn
    spriorpar(13,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".alpha2",priorarray)
    call checkn
    spriorpar(14,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".La2",priorarray)
    call checkn
    spriorpar(15,:) = priorarray(:)
  end subroutine setldde15params




  subroutine setladeparams
    if (trim(priortype) .eq. 'flat') then
       keyword = "ladelimits"
    else
       keyword = "ladecauchygamma"
    end if


    call fson_get(pars,trim(keyword)//".A",priorarray)
    call checkn
    spriorpar(1,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma1",priorarray)
    call checkn
    spriorpar(2,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".gamma2",priorarray)
    call checkn
    spriorpar(3,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".Lstar",priorarray)
    call checkn
    spriorpar(4,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".zc",priorarray)
    call checkn
    spriorpar(5,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".p1",priorarray)
    call checkn
    spriorpar(6,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".p2",priorarray)
    call checkn
    spriorpar(7,:) = priorarray(:)
    call fson_get(pars,trim(keyword)//".d",priorarray)
    call checkn
    spriorpar(8,:) = priorarray(:)
  end subroutine setladeparams


  subroutine checkn
    integer :: n1,n2
    
    n1 = size(priorarray)
    n2 = size(spriorpar(1,:))

    if (n1 /= n2) then
       write (*,*) 'Wrong number of items in prior limits in config file.'
       stop
    end if

  end subroutine checkn



     
     !                    ! A	gamma1	gamma2	Lstar
     ! !	   zc	p1	p2	alfa	La	
     ! spriorran(1:9,1) = [ 1.0e-01,3.0e-01,3.0e-01,4.1e+01, &
     !      1.0e-02,-1.0e+01,-1.0e+01,-1.0e+00,4.1e+01 ]

     ! spriorran(1:9,2) = [ 1.2e+01,3.0e+00,5.0e+00,4.7e+01, &
     !      5.0e+00,1.0e+01,1.0e+01,3.0e+00,4.7e+01 ]


     !                   ! A      gamma1  gamma2  Lstar
     ! !      zc      p1      p2      d
     ! spriorran(1:8,1) = [ 1.0e-01,-3.0,-3.0,4.1e+01, &
     !      1.0e-02,-1.0e+01,-1.0e+01,-1.0 ]

     ! spriorran(1:8,2) = [ 1.2e+01, 5.0, 5.0,4.7e+01, &
     !      5.0e+00,1.0e+01,1.0e+01,  5.0 ]






end module lfmnconfig
