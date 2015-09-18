module binarysearch

! binary search
!
! SYNOPSIS
!
! idx = bsearch(1d_real_array,value)

use precision

implicit none

contains


pure integer function nearest (array,val)
  real(kind=rkind), intent(in) :: array(:)
  real(kind=rkind), intent(in) :: val

  integer :: i,imax

  i = binsearch(array,val)
  !write (*,*) 'got ',i,array(i)
  !write (*,*) (array(i),i=1,size(array))


  imax = size(array)

  if (i==imax) then
     nearest = i
  elseif (val-array(i) < array(i+1)-val) then
     nearest = i
  else
     nearest = i+1
  endif
end function nearest


pure integer function binsearch (array,val)
  real(kind=rkind), intent(in) :: array(:)
  real(kind=rkind), intent(in) :: val

  binsearch = bsearch(array,val,1,size(array))
end function binsearch


pure recursive function bsearch (array,val,i1,i2)  result (idx)
  real(kind=rkind), intent(in) :: array(:)
  real(kind=rkind), intent(in) :: val
  integer, intent(in) :: i1,i2
  integer :: idx

  !real    :: middle
  integer :: i,imax

  i = i1 + (i2-i1) / 2

  !print *, i
  !middle = array(i)

  if (i1 == i2) then
     idx = i
  elseif (val<array(i)) then
     idx = bsearch(array,val,i1,i)
  elseif (val>array(i+1)) then
     idx = bsearch(array,val,i+1,i2)
  else
     idx = i
  endif

end function bsearch


end module

