module gehrels

  real,private :: glow(0:50),ghigh(0:50)

  ! poisson single sided upper and lower limits, from Gehrels 1986
  data ghigh/1.841,3.300,4.638,5.918,7.163,8.382,9.584,10.77,11.95 &
    ,13.11,14.27,15.42,16.56,17.70,18.83,19.96,21.08,22.20,23.32   &
    ,24.44,25.55,26.66,27.76,28.87,29.97,31.07,32.16,33.26,34.35   &
    ,35.45,36.54,37.63,38.72,39.80,49.89,41.97,43.06,44.14,45.22   &
    ,46.30,47.38,48.46,49.53,50.61,51.68,52.76,53.83,54.90,55.98   &
    ,57.05,58.12/

  data glow/0,0.173,0.708,1.367,2.086,2.840,3.620,4.419,5.232     &
      ,6.057,6.891,7.734,8.585,9.441,10.30,11.17,12.04,12.92      &
      ,13.80,14.68,15.57,16.45,17.35,18.24,19.14,20.03,20.93      &
      ,21.84,22.74,23.65,24.55,25.46,26.37,27.28,28.20,29.11      &
      ,30.03,30.94,31.86,32.78,33.70,34.62,35.55,36.47,37.39      &
      ,38.32,39.24,40.17,41.10,42.02,42.95/
      
contains


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
