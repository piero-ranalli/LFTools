program main

  use startup
  use params
  use nestwrapper

  use lfmnconfig

  implicit none

  call configure  ! parse config file, allocate arrays, read catalogues, set priors etc
  

  allocate( scaledparams(numparams) )
  sdim = numparams
  nest_nPar = numparams
  allocate( nest_pWrap(sdim) )

  
  !no parameters to wrap around
  nest_pWrap=0

  call nest_Sample
  stop
end program
