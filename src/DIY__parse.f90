Module DIY__parse
private
public :: split, tokenize
interface tokenize
   module procedure :: split_first_last, split_pos, split_tokens
end interface tokenize
interface split
   module procedure :: split_first_last, split_pos, split_tokens
end interface split
contains

pure subroutine split_tokens(string, set, tokens, separator)
! Splits a string into tokens using characters in set as token delimiters.
! If present, separator contains the array of token delimiters.
character(*), intent(in)                      :: string
character(*), intent(in)                      :: set
character(:), allocatable, intent(out)        :: tokens(:)
character, allocatable, intent(out), optional :: separator(:)

integer, allocatable                          :: first(:), last(:)
integer                                       :: n
integer                                       :: imax
! AUTHOR  : Milan Curcic, "milancurcic@hey.com"
! LICENSE : MIT
! VERSION : version 0.1.0, copyright 2020, Milan Curcic

    call split_first_last(string, set, first, last)
    ! maxval() of a zero-size array is set to a flag value not zero or length of character string
    if(size(first).eq.0)then
       imax=0
    else
       imax=maxval(last-first)+1
    endif
    allocate(character(len=imax) :: tokens(size(first)))

    do concurrent (n = 1:size(tokens))
      tokens(n) = string(first(n):last(n))
    enddo

    if (present(separator)) then
      allocate(separator(size(tokens) - 1))
      do concurrent (n = 1:size(tokens) - 1)
        separator(n) = string(first(n+1)-1:first(n+1)-1)
      enddo
    endif

end subroutine split_tokens
!===================================================================================================================================
pure subroutine split_first_last(string, set, first, last)
! Computes the first and last indices of tokens in input string, delimited
! by the characters in set, and stores them into first and last output
! arrays.
character(*), intent(in)          :: string
character(*), intent(in)          :: set
integer, allocatable, intent(out) :: first(:)
integer, allocatable, intent(out) :: last(:)

character                         :: set_array(len(set))
logical, dimension(len(string))   :: is_first, is_last, is_separator
integer                           :: n, slen
! AUTHOR  : Milan Curcic, "milancurcic@hey.com"
! LICENSE : MIT
! VERSION : version 0.1.0, copyright 2020, Milan Curcic

    slen = len(string)

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    do concurrent (n = 1:slen)
      is_separator(n) = any(string(n:n) == set_array)
    enddo

    is_first = .false.
    is_last = .false.

    if (.not. is_separator(1)) is_first(1) = .true.

    do concurrent (n = 2:slen-1)
      if (.not. is_separator(n)) then
        if (is_separator(n - 1)) is_first(n) = .true.
        if (is_separator(n + 1)) is_last(n) = .true.
      else
        if (is_separator(n - 1)) then
          is_first(n) = .true.
          is_last(n-1) = .true.
        endif
      endif
    enddo

    if (.not. is_separator(slen)) is_last(slen) = .true.

    first = pack([(n, n = 1, slen)], is_first)
    last = pack([(n, n = 1, slen)], is_last)

  end subroutine split_first_last
!===================================================================================================================================
pure subroutine split_pos(string, set, pos, back)
! If back is absent, computes the leftmost token delimiter in string whose
! position is > pos. If back is present and true, computes the rightmost
! token delimiter in string whose position is < pos. The result is stored
! in pos.
character(*), intent(in)      :: string
character(*), intent(in)      :: set
integer, intent(in out)       :: pos
logical, intent(in), optional :: back

logical                       :: backward
character                     :: set_array(len(set))
integer                       :: n, result_pos
! AUTHOR  : Milan Curcic, "milancurcic@hey.com"
! LICENSE : MIT
! VERSION : version 0.1.0, copyright 2020, Milan Curcic

    !TODO use optval when implemented in stdlib
    !backward = optval(back, .false.)
    backward = .false.
    if (present(back)) backward = back

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    if (backward) then
      result_pos = 0
      do n = pos - 1, 1, -1
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    else
      result_pos = len(string) + 1
      do n = pos + 1, len(string)
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    endif

    pos = result_pos

end subroutine split_pos

end Module DIY__parse
