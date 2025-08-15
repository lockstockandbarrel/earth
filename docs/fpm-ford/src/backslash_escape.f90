program backslash_escape 
use,intrinsic :: iso_fortran_env, only: output_unit
implicit none
integer,parameter :: ucs4 = selected_char_kind('ISO_10646')
character(kind=ucs4,len=:),allocatable :: str

   ! EXTENSION:
   str = ucs4_'Unicode character: \u263B'

   open (output_unit, encoding='utf-8')
   print '(a)', str
   print '(a)', ucs4_'Unicode character: \U0000263B'
end program backslash_escape 
