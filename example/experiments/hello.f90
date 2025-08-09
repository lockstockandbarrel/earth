program testit
use iso_fortran_env, only : output_unit
use M_utf8, only : utf8_to_ucs4, ucs4_to_utf8_via_io, utf8_to_ucs4_via_io
implicit none
integer,parameter                       :: ascii = selected_char_kind ("ascii")
integer,parameter                       :: ucs4 = selected_char_kind("ISO_10646")
character(len=:,kind=ucs4),allocatable  :: str
character(len=1,kind=ucs4)              :: glyph
character(len=*),parameter              :: all='(*(g0))'
character(len=:,kind=ascii),allocatable :: aline
character(len=:,kind=ucs4),allocatable  :: uline
integer                                 :: i

   open (output_unit, encoding='UTF-8')
   
   ! standard method
   str  = ucs4_'Hello World and Ni Hao -- ' &
      // char (int (z'4F60'), ucs4)         &
      // char (int (z'597D'), ucs4)
   write (*,*) str

   ! converting pseudo-utf8 to ucs4
   str= utf8_to_ucs4('Hello World and Ni Hao -- 你好')
   write (*,*) str

   print all
   print all, 'intrinsics work with ucs4:'
   print all, 'slice glyph 27:',str(27:27)
   print all, 'length        :',len(str)
   print all, 'bytes         :',storage_size(str)/8
   glyph=str(27:27)
   print all, 'index         :',index(str,glyph)
   write(*,'(a,i0,",z''",z0,"''")') 'ichar         :',ichar(glyph),glyph
   glyph=char(int(z'597D'),kind=ucs4)
   print all, 'char          :',glyph
   print all, 'repeat        :',repeat(str(27:28),3)

   ! SHOULD WRITE INTO ASCII CREATE RAW UTF8 OR WHAT?
   print all
   print all, 'internal write into ascii'
   aline=repeat(' ',len(str)*4)
   write(aline,all)str
   aline=trim(aline)
   print all, 'length        :',len(aline)
   print all, 'bytes         :',storage_size(aline)/8
   print all,aline

   ! SHOULD WRITE INTO UCS4 DO ANYTHING DIFFERENT?
   print all
   print all, 'internal write into ucs4'
   uline=repeat(' ',len(str))
   write(uline,all)str
   uline=trim(uline)
   print all, 'length        :',len(uline)
   print all, 'bytes         :',storage_size(uline)/8
   print all,uline

   print all
   print all, 'raw utf8' 
   print all, 'length        :',len('Hello World and Ni Hao -- 你好')
   print all, 'bytes         :',storage_size( 'Hello World and Ni Hao -- 你好')/8

   ! WHAT SHOULD ASSIGN DO?
   print all
   print all, 'assign' 
   aline=uline
   print all, 'length        :',len(aline)
   print all, 'bytes         :',storage_size(aline)/8
   print all,aline

   print all
   print all, 'to be or not to be' 
   ! Ikiru ka shinu ka
   ! "to live or die"
   str= utf8_to_ucs4('生きるか死ぬか')
   print all, 'ucs4 string:',str
   print all, 'reverse:    ',(str(i:i),i=len(str),1,-1)

   print all
   print all, 'ucs4 to utf8' 
   str= utf8_to_ucs4_via_io('生きるか死ぬか')
   aline = ucs4_to_utf8_via_io(str)
   print all, merge('PASSED','FAILED',aline == '生きるか死ぬか')

end program testit
