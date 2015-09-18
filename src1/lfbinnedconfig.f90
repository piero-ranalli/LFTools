module lfbinnedconfig  ! configuration for lf-binned

  ! shortened version of lfmnconfig.f90

  use fson
  use fson_value_m, only: fson_value_count, fson_value_get
  
  implicit none
  integer, private :: i, ncats
  character(len=256), private :: config
  type(fson_value), pointer, private :: pars,catarray,catitem

  ! public
  character(len=256), allocatable :: catlist(:),arealist(:)
  real :: myH0, myOM, myOL

  
contains

  subroutine configure

    ! get configuration file from command line, and
    ! get parameters from configuration file
    call get_command_argument(1,config)
    if (len_trim(config)==0) then
       write (*,*) 'Please specify a configuration file.'
       stop
    end if

    pars => fson_parse(config)

    call fson_get(pars,"catalogues",catarray)

    ! read catalogues
    ncats = fson_value_count(catarray)
    allocate( catlist(ncats), arealist(ncats) )
    do i=1, ncats
       catitem => fson_value_get(catarray, i)
       call fson_get(catitem,"cat",catlist(i))
       call fson_get(catitem,"area",arealist(i))
    end do


    ! get cosmology
    call fson_get(pars,"cosmology.H0",myH0)
    call fson_get(pars,"cosmology.OL",myOL)
    call fson_get(pars,"cosmology.OM",myOM)
    
  end subroutine configure




     
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






end module lfbinnedconfig
