program assign_exe
use M_unicode, only : len, len_trim, repeat, trim, adjustr, adjustl
use M_unicode, only : character, range 
use M_unicode, only : assignment(=), unicode_type
character(len=*),parameter   :: g='(*(g0))'
character(len=:),allocatable :: aline
type(unicode_type)           :: uline, substring, uline1, uline2, uline3
character(len=*),parameter   :: smiley='😃'
integer,allocatable          :: codes(:)
character(len=:),allocatable :: glyphs

   aline="Доки не впріти, доти не вміти."

   write(*,g)'123456789012345678901234567890'
   write(*,g)aline
   write(*,g)'length in bytes is: ',len(aline)
   uline=aline
   write(*,g)'length in glyphs is: ',len(uline)

   write(*,g)'string is: ',character(uline) 
   write(*,g)'third word is: ',character(uline,9,14) ! substring

   substring=range(uline,17,29)
   write(*,g)'string is: ',character(substring) 

   uline=repeat(smiley,30)
   write(*,g) character(uline)

   write(*,g) len_trim(uline)
   uline=aline//'      '
   write(*,g) len_trim(uline)

   uline=[32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288]
   write(*,g)'spaces:',character(uline),len(uline),len_trim(uline)

   uline=[32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288]
   uline=trim(uline)
   write(*,g)'trim:','[',character(uline),']'

   uline='    this  is just a    string        '
   write(*,g)'adjustr:','[',character(uline),'] << starts with'
   write(*,*)'adjustr:','[',character(adjustr(uline)),']'
   uline=character(uline)
   write(*,*)'adjustr:','[',character(adjustr(uline)),'] << repeat'

   uline1=''
   write(*,g)'adjustr:','[',character(adjustr(uline1)),'] << call with null string'
   uline1='        '
   write(*,g)'adjustr:','[',character(adjustr(uline1)),'] << call with blank string'

   uline1='abcdefghij'
   write(*,g)'adjustr:','[',character(adjustr(uline1)),'] << start with full string'

   write(*,g)
   uline='    this  is just a    string        '
   write(*,g)'adjustl:','[',character(uline),']'
   write(*,g)'adjustl:','[',character(adjustl(uline)),']'

   uline1=''
   write(*,g)'adjustl:','[',character(adjustl(uline1)),']'

   uline1='abcdefghij'
   write(*,g)'adjustl:','[',character(adjustl(uline1)),']'

   !write(*,g)uline%codes
   !write(*,g)uline
end program assign_exe
