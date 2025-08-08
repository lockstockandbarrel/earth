program testit
use iso_fortran_env, only : stdout=>output_unit
use M_utf8, only : ucs4_to_utf8
implicit none
integer,parameter                       :: ucs4 = selected_char_kind("ISO_10646")
character(len=:,kind=ucs4),allocatable  :: ustr
character(len=:),allocatable            :: astr
integer                                 :: err

   open (stdout, encoding='UTF-8')
   
   ! standard method
   ustr  = ucs4_'Hello World and Ni Hao -- ' &
      // char (int (z'4F60'), ucs4)         &
      // char (int (z'597D'), ucs4)
   write (stdout,*) ustr

   write (stdout,*)merge('PASSED','FAILED',storage_size(ustr) /= storage_size('Hello World and Ni Hao -- 你好') )
   astr=ucs4_to_utf8(ustr,err)
   write (stdout,*)merge('PASSED','FAILED',astr == 'Hello World and Ni Hao -- 你好')

   write(stdout,*)ustr
   write(stdout,*)astr


end program testit
