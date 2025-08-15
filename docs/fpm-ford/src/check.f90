program check
use iso_fortran_env, only : output_unit
use M_utf8, only : utf8_to_codepoints, utf8_to_ucs4, ucs4_to_utf8, utf8_to_ucs4_via_io, ucs4_to_utf8_via_io
implicit none
integer,parameter          :: ucs4 = selected_char_kind ('ISO_10646')
integer,parameter          :: ascii = selected_char_kind ("ascii")
!
character(len=*),parameter :: upagain="七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。"
! Romanization:
! Nanakorobi yaoki. Koronde mo mata tachiagaru. Kujikezu ni mae o muite aruite ikou.
! or English translation
! "Fall seven times, stand up eight. Even if you fall down, you will get up again. Don't be discouraged, just keep walking forward."
!
character(len=1,kind=ucs4)             :: stop
character(len=:,kind=ucs4),allocatable :: ustr
character(len=:),allocatable           :: astr
integer                                :: total
integer                                :: err

! OUTPUT:七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。
 character(len=*,kind=ucs4),parameter :: boz= &
 char(int(z'4E03'),kind=ucs4)// char(int(z'8EE2'),kind=ucs4)// char(int(z'3073'),kind=ucs4)// &
 char(int(z'516B'),kind=ucs4)// char(int(z'8D77'),kind=ucs4)// char(int(z'304D'),kind=ucs4)// &
 char(int(z'3002'),kind=ucs4)// char(int(z'8EE2'),kind=ucs4)// char(int(z'3093'),kind=ucs4)// &
 char(int(z'3067'),kind=ucs4)// char(int(z'3082'),kind=ucs4)// char(int(z'307E'),kind=ucs4)// &
 char(int(z'305F'),kind=ucs4)// char(int(z'7ACB'),kind=ucs4)// char(int(z'3061'),kind=ucs4)// &
 char(int(z'4E0A'),kind=ucs4)// char(int(z'304C'),kind=ucs4)// char(int(z'308B'),kind=ucs4)// &
 char(int(z'3002'),kind=ucs4)// char(int(z'304F'),kind=ucs4)// char(int(z'3058'),kind=ucs4)// &
 char(int(z'3051'),kind=ucs4)// char(int(z'305A'),kind=ucs4)// char(int(z'306B'),kind=ucs4)// &
 char(int(z'524D'),kind=ucs4)// char(int(z'3092'),kind=ucs4)// char(int(z'5411'),kind=ucs4)// &
 char(int(z'3044'),kind=ucs4)// char(int(z'3066'),kind=ucs4)// char(int(z'6B69'),kind=ucs4)// &
 char(int(z'3044'),kind=ucs4)// char(int(z'3066'),kind=ucs4)// char(int(z'3044'),kind=ucs4)// &
 char(int(z'3053'),kind=ucs4)// char(int(z'3046'),kind=ucs4)// char(int(z'3002'),kind=ucs4)

   total = 0

   write(*,*)'## utf8_to_ucs4'
   ustr=utf8_to_ucs4(upagain)

   write(*,*)'if file is not open for utf-8 encoding automatic conversion does not occur'
   write(*,*)'utf8:',upagain  ! these are the bytes to represent the utf-8 characters
   write(*,*)'ucs4:',ustr     ! this is 4-byte unicode that needs converted. Does it print as noise or all "?"?
   write(*,*)
   write(*,*)'encoding can be altered on an open file'
   open (output_unit, encoding='UTF-8')
   write(*,*)'utf8:',upagain
   write(*,*)'ucs4:',ustr     ! so this should get "automatically" printed properly now
   write(*,*)
   stop=ustr(7:7)
   call checkit('check len() is 36 :',len(ustr) == 36)
   call checkit('check index       :',index(ustr,stop,kind=ucs4,back=.false.) == 7)
   call checkit('check index       :',index(ustr,stop,back=.true.,kind=ucs4) == len(ustr) )
   call checkit('storage_size      :',storage_size(ustr) == len(ustr)*4*8 )

   write(*,*)'## ucs4_to_utf8'
   write(*,*)
   write(*,*)'converted by external routine:',ucs4_to_utf8(boz)     ! it is converted by the routine
   write(*,*)'converted because file is encoded to utf-8:',boz                   ! converted by standard Fortran

   ! standard method. Note ASCII one-byte characters become 4-byte characters, but multi-byte characters are not recognized
   ustr  = ucs4_'Hello World and Ni Hao -- ' &
      // char (int (z'4F60'), ucs4)         &
      // char (int (z'597D'), ucs4)
   astr=ucs4_to_utf8(ustr,err)
   call checkit('convert to ASCII bytes representing utf8',astr == 'Hello World and Ni Hao -- 你好')

   if(total.ne.0)then
      write(*,*)total,'failures'
      stop 1
   endif

contains

subroutine checkit(label,test)
character(len=*),intent(in) :: label
logical,intent(in)          :: test
   write(*,*)merge('PASSED','FAILED',test),' ',label
   if(.not.test)total=total+1
end subroutine checkit

end program check
