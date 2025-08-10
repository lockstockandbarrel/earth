program internal_io
use iso_fortran_env, only : stdout=>output_unit
implicit none
integer,parameter                       :: ascii = selected_char_kind ("ascii")
integer,parameter                       :: ucs4 = selected_char_kind("ISO_10646")
character(len=1,kind=ucs4)              :: glyph
character(len=*),parameter              :: all='(*(g0))'
character(len=:,kind=ascii),allocatable :: aline
character(len=:,kind=ascii),allocatable :: astr
character(len=:,kind=ucs4),allocatable  :: uline
character(len=:,kind=ucs4),allocatable  :: ustr
integer                                 :: i

   open (stdout, encoding='UTF-8')
   
   print all, 'unicode UCS-4 string'
   ustr  = ucs4_'Hello World and Ni Hao -- ' // char(int(z'4F60'),ucs4) // char(int(z'597D'),ucs4)
   write (*,*) ustr
   print all, 'length  :',len(ustr)
   print all, 'bytes   :',storage_size(ustr)/8

   print all, 'ASCII bytes'
   astr= 'Hello World and Ni Hao -- 你好'
   write (*,*) astr
   print all, 'length  :',len(astr)
   print all, 'bytes   :',storage_size(astr)/8

   ! SHOULD WRITE INTO ASCII CREATE RAW UTF8 OR WHAT?
   print all, 'UCS4 characters written to ASCII internal file'
   aline=repeat(' ',len(ustr))
   write(aline,all)ustr
   aline=trim(aline)
   print all, 'length        :',len(aline)
   print all, 'bytes         :',storage_size(aline)/8
   print all,aline

   ! SHOULD WRITE INTO UCS4 DO ANYTHING DIFFERENT?
   print all
   print all, 'UCS4 characters written to UCS4 internal file'
   uline=repeat(' ',len(ustr))
   write(uline,all)ustr
   uline=trim(uline)
   print all, 'length        :',len(uline)
   print all, 'bytes         :',storage_size(uline)/8
   print all,uline

   ! SHOULD WRITE INTO ASCII CREATE RAW UTF8 OR WHAT?
   print all, 'ASCII characters into ASCII internal file'
   aline=repeat(' ',len(astr))
   write(aline,all)astr
   aline=trim(aline)
   print all, 'length        :',len(aline)
   print all, 'bytes         :',storage_size(aline)/8
   print all,aline

   ! SHOULD WRITE INTO UCS4 DO ANYTHING DIFFERENT?
   print all
   print all, 'ASCII characters into UCS4 internal file'
   uline=repeat(' ',len(astr))
   write(uline,all)astr
   uline=trim(uline)
   print all, 'length        :',len(uline)
   print all, 'bytes         :',storage_size(uline)/8
   print all,uline
   write(stdout,all)(ichar(uline(i:i)),",",i=1,len(uline))
   print all, 'And back again'
   write(stdout,all)'before:',astr
   write(stdout,all)(ichar(astr(i:i)),",",i=1,len(astr))
   read(uline,'(a)')astr
   write(stdout,all)'after:',astr
   write(stdout,all)(ichar(astr(i:i)),",",i=1,len(astr))

end program internal_io
