program uni_to_ftn
! @(#) take command line argument utf-8 text and generate Fortran statement that represents the string as char(3f) calls
use, intrinsic :: iso_fortran_env, only : output_unit
use M_utf8, only : utf8_to_ucs4
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter             :: form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter             :: g= '(*(g0))'
integer                                :: length
integer                                :: i
character(len=:),allocatable           :: aline
character(len=:),allocatable           :: command_line
character(len=:,kind=ucs4),allocatable :: ustr
   open (output_unit, encoding='UTF-8')
   call get_command(length=length)                 ! get command line length
   allocate(character(len=length) :: command_line) ! allocate string big enough to hold command line
   call get_command(command=command_line)          ! get command line as a string
   call get_command_argument(0,length=length)      ! remove argument 0
   command_line=adjustl(command_line(length+1:))
   ustr=utf8_to_ucs4(command_line)
   write(*,g) 'program testit'
   write(*,g) 'use, intrinsic :: iso_fortran_env, only : output_unit'
   write(*,g) "integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')"
   write(*,g) '! OUTPUT:',command_line
   write(*,g) 'character(len=*,kind=ucs4),parameter :: variable = &'
   write(*,form)(ustr(i:i),i=1,len(ustr))
   write(*,g) "   open (output_unit, encoding='utf-8')"
   write(*,g) "   write(output_unit,'(a)' )variable"
   write(*,g) "end program testit"
end program uni_to_ftn
