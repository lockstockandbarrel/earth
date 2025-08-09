program unifile_to_ftn
! @(#) take command line argument utf-8 text and generate Fortran statement that represents the string as char(3f) calls
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter             :: form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter             :: g= '(*(g0))'
character(len=80)                      :: count
integer                                :: i, j, iostat
character(len=4096,kind=ucs4)          :: uline
   open (stdin, encoding='UTF-8')
   open (stdout, encoding='UTF-8')
   write(stdout,g) 'program testit'
   write(stdout,g) 'use, intrinsic :: iso_fortran_env, only : output_unit'
   write(stdout,g) "integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')"
   write(stdout,g) "   open (output_unit, encoding='utf-8')"
   do j=1,huge(0)-1
      read(stdin,'(a)',iostat=iostat)uline
      if(iostat.ne.0)exit
      write(count,g) "variable_",j,"= &"
      write(stdout,g) 'block'
      write(stdout,g) '! Unicode code points for ',trim(uline)
      write(stdout,g) 'character(len=*,kind=ucs4),parameter :: '//trim(count)
      write(stdout,form)(uline(i:i),i=1,len_trim(uline))
      write(stdout,g) "   write(output_unit,'(a)' )variable_",j
      write(stdout,g) 'endblock'
   enddo
   write(stdout,g) "end program testit"
end program unifile_to_ftn
