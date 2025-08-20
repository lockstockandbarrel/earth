program test_M_unicode
use iso_fortran_env, only : output_unit
use M_unicode, only : adjustl, adjustr, trim
use M_unicode, only : character
use M_unicode, only : assignment(=), unicode_type, operator(//)
implicit none
integer,parameter          :: ascii = selected_char_kind ("ascii")
character(len=*),parameter :: g0='(*(g0))'
character(len=*),parameter :: gx='(*(g0,1x))'
!
character(len=*),parameter :: upagain="七転び八起き。転んでもまた立ち上がる。くじけずに前を向いて歩いていこう。"
! Romanization:
! Nanakorobi yaoki. Koronde mo mata tachiagaru. Kujikezu ni mae o muite aruite ikou.
! or English translation
! "Fall seven times, stand up eight. Even if you fall down, you will get up again. Don't be discouraged, just keep walking forward."
!
character(len=:),allocatable   :: astr
type(unicode_type)             :: ut_str
type(unicode_type)             :: smiley
integer                        :: total
integer                        :: err

   smiley='😃'
   total = 0

   write(*,g0)'encoding can be altered on an open file'
   open (output_unit, encoding='UTF-8')

   write(*,g0)
   astr='Hello World and Ni Hao -- 你好'
   ut_str=astr
   call checkits('convert to ASCII bytes',astr,ut_str%bytes(),transfer('Hello World and Ni Hao -- 你好',['A']))

   astr="  this is a string    "
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'this is a string      ')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'      this is a string')
   call checkit('trim',astr,character(trim(ut_str%trim())),'  this is a string')
   call check('len_trim',ut_str%len_trim().eq.18)
   call check('len',ut_str%len().eq.22)

   astr="  "
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'  ')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'  ')
   call checkit('trim',astr,character(trim(ut_str%trim())),'')
   call check('len_trim',ut_str%len_trim().eq.0)
   call check('len',ut_str%len().eq.2)

   astr=""
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'')
   call checkit('trim',astr,character(trim(ut_str%trim())),'')
   call check('len_trim',ut_str%len_trim().eq.0)
   call check('len',ut_str%len().eq.0)

   astr="ALLFULL"
   ut_str=astr
   call checkit('adjustl',astr,character(ut_str%adjustl()),'ALLFULL')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'ALLFULL')
   call checkit('trim',astr,character(trim(ut_str%trim())),'ALLFULL')
   call check('len_trim',ut_str%len_trim().eq.7)
   call check('len',ut_str%len().eq.7)

   ut_str=[32,32,int(z'1F603'),32,32,32]
   astr=character(ut_str)
   call checkit('adjustl',astr,character(ut_str%adjustl()),'😃     ')
   call checkit('adjustr',astr,character(ut_str%adjustr()),'     😃')
   call checkit('trim',astr,character(trim(ut_str%trim())),'  😃')
   call check('len_trim',ut_str%len_trim().eq.3)
   call check('len',ut_str%len().eq.6)

   if(total.ne.0)then
      write(*,g0)total,'failures'
      stop 1
   endif

   ut_str=smiley // ' and ' // smiley // 'and' // smiley // smiley // 'is it'
   astr='😃 and 😃and😃😃is it'
   call checkit('concatenation',astr,character(ut_str), '😃 and 😃and😃😃is it')

contains

subroutine checkit(label,aline,answer,expected)
character(len=*),intent(in) :: label
character(len=*),intent(in) :: aline
character(len=*),intent(in) :: answer
character(len=*),intent(in) :: expected
   write(*,g0)merge('PASSED','FAILED',answer.eq.expected),' ',label,':[',aline,'][',answer,'][',expected,']'
   if(answer.ne.expected)total=total+1
end subroutine checkit

subroutine checkits(label,aline,answer,expected)
character(len=*),intent(in) :: label
character(len=*),intent(in) :: aline
character(len=*),intent(in) :: answer(:)
character(len=*),intent(in) :: expected(:)
   write(*,g0)merge('PASSED','FAILED',all(answer.eq.expected)),' ',label,':[',aline,'][',answer,'][',expected,']'
   if(all(answer.ne.expected))total=total+1
end subroutine checkits

subroutine check(label,test)
character(len=*),intent(in) :: label
logical,intent(in)          :: test
   write(*,g0)merge('PASSED','FAILED',test),' ',label
   if(.not.test)total=total+1
end subroutine check

end program test_M_unicode
