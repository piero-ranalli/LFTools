module fileutils
  
contains

  function lines_in_file (filename,skipcomments) result (n)
    character(*), intent(in) :: filename
    logical, intent(in), optional :: skipcomments
    logical :: skip ! comments
    integer :: n,u
    character(80) :: row
    
    if (present(skipcomments)) then
       skip = skipcomments
    else
       skip = .true.
    end if


    open (newunit=u, file=filename, status='old')
    n=0
    do
       read (u,*,end=999) row

       if (skip) then
          ! remove leading spaces
          row = adjustl(row)
          ! skip line if it's a comment
          if (index(row,'#') == 1)  cycle
       end if

       ! (blank lines are automatically skipped by Fortran)
       
       n = n+1

    end do


999 close(u)

    return

  end function lines_in_file


  function iscomment(row)
    character(*), intent(in) :: row
    logical :: iscomment

    if (index(adjustl(row),'#') == 1) then
       iscomment = .true.
    else
       iscomment = .false.
    end if
  end function iscomment

  
end module fileutils
