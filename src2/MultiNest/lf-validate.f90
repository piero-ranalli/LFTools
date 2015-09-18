program LF_validate

  ! use: ./lf-validate lss-cdfs-cosmos.json validate.json
  ! where lss-cdfs-cosmos.json is the same used for all-surveys estimate
  ! and validate.json has the following structure:
  !    {
  !      "alldata" : {
  !              "pew"     : "lss-cdfs-cosmos-post_equal_weights.dat"
  !      },
  !      "crossvalidation" : [
  !           {
  !              "cat"     : "lss.dat",
  !              "area"    : "lssarea.dat",
  !              "holdout" : "lss-post_equal_weights.dat",
  !              "rest"    : "cdfs-cosmos-post_equal_weights.dat"
  !           },
  !           {
  !              "cat"     : "cdfs.dat",
  !              "area"    : "cdfsarea.dat",
  !              "holdout" : "cfs-post_equal_weights.dat",
  !              "rest"    : "lsss-cosmos-post_equal_weights.dat"
  !           },
  !           {
  !              "cat"     : "cosmos.dat",
  !              "area"    : "cosmosarea.dat",
  !              "holdout" : "cosmos-post_equal_weights.dat",
  !              "rest"    : "lss-cdfs-post_equal_weights.dat"
  !           }
  !      ]
  !   }
  !
  ! The program will take the info on catalogue, evolution and
  ! parameter scaling from the first json file (lss-cdfs-cosmos.json)
  ! and use that, together with the post-equal-weights (pew) specified
  ! in validate.json to compute WAIC. Then it will use the
  ! holdout/rest pairs of posterior samples to do cross-validation.

  use m_option_parser
  use lfvalidateconfig
  use lfmnconfig
  use startup
  use params
  use lppd_etc
  use precision

  implicit none
  
  type(option_t), allocatable :: program_options(:)
  logical :: do_waic = .true., in_cmd_line, ms_in_cmd_line
  integer :: maxsamples
  type(tlppd) :: lppd
  character(280) :: pew,cvconfigfile
  real(kind=rkind) :: waic

  ! read command line parameters
  call set_parser_options ( with_equal_sign = .true. )
  allocate( program_options(2) )
  call set_option ( program_options , "--waic" , "" , .true.  &
       , "Calc WAIC" )
  call set_option ( program_options , "--maxsamples" , "" , 0  &
       , "Max samples" )
  call parse_options ( program_options )
  call get_option_value ( program_options , "--waic" ,   &
       do_waic , in_cmd_line )
  call get_option_value ( program_options , "--maxsamples" ,   &
       maxsamples, ms_in_cmd_line )


  ! we use here the same infrastructure of lf-mn ; this is not
  ! particularly elegant but will do until we refactor...
  
  call configure  ! parse config file, allocate arrays, read
                  ! catalogues, set priors etc

  allocate( scaledparams(numparams) )
  sdim = numparams
  nest_nPar = numparams

  ! get parameters from configuration file
  call get_command_argument(2,cvconfigfile)
  if (len_trim(cvconfigfile)==0) then
     write (*,*) 'Please specify a configuration file for cross-validation.'
     stop
  end if

  call validateconfig%new(cvconfigfile)

  ! startup LPPD
  call lppd%new( ncvcats=validateconfig%ncvcats )

  if (ms_in_cmd_line) then
     call lppd%setallsurveys( validateconfig%alldatapew, maxsamples )
  else
     call lppd%setallsurveys( validateconfig%alldatapew )
  end if

  write (*,*) 'AIC = ',lppd%aic()

  ! WAIC is based on all data, and on posterior from fit to all data, so it only
  ! needs the all-survey config
  if (do_waic)  write (*,*) ' WAIC = ',lppd%waic()

  ! ! CV instead needs more work, since more catalogues have to be allocated
  ! do i=1, validateconfig%ncvcats
  !    call lppd%readcv( i, validateconfig%cvcat(i),   &
  !                         validateconfig%cvarea(i),  &
  !                         validateconfig%holdout(i),  &
  !                         validateconfig%rest(i) )
  ! end do
  ! write (*,*) ' CV = ',lppd%calc_cv

end program LF_validate
