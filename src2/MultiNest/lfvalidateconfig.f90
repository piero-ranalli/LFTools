!> configuration module for lf-validate
module lfvalidateconfig

  use fson
  use fson_value_m, only: fson_value_count, fson_value_get
  
  implicit none

!> type containing filenames of data needed for cross-validation
  type :: lfvalconf
!>   number of cat/area/holdout/res tuples
     integer :: ncvcats
!>   post-equal-weights (pew) for all data together
     character(len=256) :: alldatapew
!>   arrays of cat,area,holdout pew,rest pew
     character(len=256), dimension(:), allocatable :: cvcat,cvarea,holdout,rest
   contains
     procedure :: new => lfvalidateconfig_new
  end type lfvalconf

  type(lfvalconf) :: validateconfig
  
contains

!> read validate-relevant information from json file and store filenames
  subroutine lfvalidateconfig_new (this, config)
    class(lfvalconf) :: this
    character(len=256) :: config

    integer :: i
    type(fson_value), pointer :: pars,catarray,catitem

    
    pars => fson_parse(config)

    call fson_get(pars,"alldata.pew",this%alldatapew)
    call fson_get(pars,"crossvalidation",catarray)

    ! cv catalogues
    this%ncvcats = fson_value_count(catarray)
    allocate( this%cvcat(this%ncvcats),   this%cvarea(this%ncvcats), &
              this%holdout(this%ncvcats), this%rest(this%ncvcats)      )
    
    do i=1, this%ncvcats
       catitem => fson_value_get(catarray, i)
       call fson_get(catitem,"cat",    this%cvcat(i)  )
       call fson_get(catitem,"area",   this%cvarea(i) )
       call fson_get(catitem,"holdout",this%holdout(i))
       call fson_get(catitem,"rest",   this%rest(i)   )
    end do

  end subroutine lfvalidateconfig_new
    
end module lfvalidateconfig
