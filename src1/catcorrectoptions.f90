module catcorrectoptions

  use m_option_parser
  use photoz
  
  implicit none
  
  ! command line parameters
  type(option_t)    , allocatable :: program_options(:)
  character*256 infile, outfile, completenessfile
  logical do_nhcorr, do_photozpdf, do_range, do_compl
  logical in_cmd_line, do_selectprob
  logical save_abs_corr
  real :: myH0, myOM, myOL
  integer pdfstart,pdfstop
  character (len=photozpathlen) :: pdfpath
  integer err
  real :: gamma

  ! end of command line pars



contains

  subroutine parseopt
    implicit none

    ! read command line parameters
    call set_parser_options ( with_equal_sign = .true. )

    allocate( program_options(15) )

    call set_option ( program_options , "--infile" , "" , 'inputcatalogue.dat'  &
         , "Input catalogue" )
    call set_option ( program_options , "--outfile" , "" , 'catalogue.lftools'  &
         , "Output catalogue" )
    call set_option ( program_options , "--range" , "" , .false.  &
         , "Define L,z limits" )
    call set_option ( program_options , "--pdfstart" , "" , 1  &
         , "Photo-z PDF starting line" )
    call set_option ( program_options , "--pdfstop" , "" , 601   &
         , "Photo-z PDF stopping line" )
    call set_option ( program_options , "--pdfpath" , "" , ''   &
         , "Photo-z PDF path" )
    call set_option ( program_options , "--nhcorr" , "" , .false.   &
         , "Correct for absorption" )
    call set_option ( program_options , "--photozpdf" , "" , .false.   &
         , "Correct for photo-z" )
    call set_option ( program_options , "--kcorrgamma" , "", 1.7    &
         , "Gamma for K-correction" )
    call set_option ( program_options , "--H0" , "" , 70.   &
         , "Hubble constant" )
    call set_option ( program_options , "--OL" , "" , .7   &
         , "Omega_Lambda" )
    call set_option ( program_options , "--OM" , "" , .3   &
         , "Omega_Matter" )
    call set_option ( program_options , "--complcorr" , "" , .false.   &
         , "Completeness correction" )
    call set_option ( program_options , "--complcorrfile" , "" , 'completeness.dat'   &
         , "Completeness correction file" )
    call set_option ( program_options , "--savenhcorr" , "" , .false.   &
         , "Save applied absorption corrections" )
    

    ! check that option keys are not set double
    ! this should only be used during development of a program.
    call check_options ( program_options , err )
    if (err /= 0) stop

    ! parse command line and return key-value pairs for all supported options:
    ! -> short (with or without value)
    ! -> long  (with or without value)
    ! -> short concatenated (with or without value)
    !
    ! e.g. :   $ ./prog -vi file_name.xml --file-unit 14 -o output.xml
    call parse_options ( program_options )

    ! attach cmdline/default options to variables
    !                         derived type  -     key       - local var - supplied or not (optional)
    call get_option_value ( program_options , "--infile" ,   &
         infile , in_cmd_line )
    call get_option_value ( program_options , "--outfile" ,   &
         outfile , in_cmd_line )
    call get_option_value ( program_options , "--range" ,   &
         do_range , in_cmd_line )
    call get_option_value ( program_options , "--pdfstart" ,   &
         pdfstart , in_cmd_line )
    call get_option_value ( program_options , "--pdfstop" ,    &
         pdfstop , in_cmd_line )
    call get_option_value ( program_options , "--pdfpath" ,    &
         pdfpath , in_cmd_line )
    call get_option_value ( program_options , "--nhcorr" ,     &
         do_nhcorr , in_cmd_line )
    call get_option_value ( program_options , "--photozpdf" ,  &
         do_photozpdf , in_cmd_line )
    call get_option_value ( program_options , "--kcorrgamma" ,  &
         gamma , in_cmd_line )
    call get_option_value ( program_options , "--H0" ,         &
         myH0 , in_cmd_line )
    call get_option_value ( program_options , "--OM" ,         &
         myOM , in_cmd_line )
    call get_option_value ( program_options , "--OL" ,         &
         myOL , in_cmd_line )
    call get_option_value ( program_options , "--complcorr" ,         &
         do_compl , in_cmd_line )
    call get_option_value ( program_options , "--complcorrfile" ,         &
         completenessfile , in_cmd_line )
    call get_option_value ( program_options , "--savenhcorr" ,         &
         save_abs_corr , in_cmd_line )

    !      write (*,*) do_nhcorr,do_fluxcorr,do_areacorr,do_photozpdf,
    !     $    do_selectprob,do_cdfs,do_lss,do_cosmos


  end subroutine parseopt

end module catcorrectoptions
