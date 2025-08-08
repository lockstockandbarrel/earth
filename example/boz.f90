program multi_line
use iso_fortran_env, only : output_unit
use M_utf8, only : utf8_to_ucs4
implicit none
integer,parameter                       :: ascii = selected_char_kind ("ascii")
integer,parameter                       :: ucs4 = selected_char_kind("ISO_10646")
integer                                 :: i, j, longest
character(len=3,kind=ucs4)              :: u3
character(len=255),parameter            :: upagain(3)=[character(len=255) :: &
 "七転び八起き。", &
 "転んでもまた立ち上がる。", &
 "くじけずに前を向いて歩いていこう。"]
character(len=4*len(upagain),kind=ucs4) :: ustr(size(upagain))
character(len=:),allocatable            :: aline
character(len=:,kind=ucs4),allocatable  :: uline
!
! Romanization:
!    Nanakorobi yaoki. Koronde mo mata tachiagaru. Kujikezu ni mae o muite aruite ikou.
! or English translation
!
!    "Fall seven times, stand up eight.
!    Even if you fall down, you will get up again.
!    Don't be discouraged, just keep walking forward."
!
   open (output_unit, encoding='UTF-8')

! convert pseudo-utf-8 ascii to actual Fortran ucs4 unicode
   longest=0 ! get longest trimmed line
   do i=1,size(upagain)
      ustr(i)=utf8_to_ucs4(upagain(i))
      longest=max(longest,len_trim(ustr(i)))
      write(*,*)'LEN=',len_trim(ustr(i))
   enddo
   write(output_unit,*)'longest=',longest

   write(output_unit,*)
   write(output_unit,*)(trim(ustr(i)),i=1,size(ustr))

   write(output_unit,*)
   write(output_unit,'(A)') (trim(ustr(i)),i=1,size(ustr))
   write(output_unit,'(*(z8.8))') ((ustr(i)(j:j),j=1,len_trim(ustr(i))),i=1,size(ustr))

   write(output_unit,*)
   aline='&
    &00004E0300008EE2000030730000516B00008D770000304D0000300200008EE20000309300003067&
    &000030820000307E0000305F00007ACB0000306100004E0A0000304C0000308B000030020000304F&
    &00003058000030510000305A0000306B0000524D0000309200005411000030440000306600006B69&
    &000030440000306600003044000030530000304600003002'
    allocate(character(len=len(aline)/8,kind=ucs4) :: uline)
!   read(aline,'(z288)') uline
   read(aline,'(*(z8.8))') (uline(i:i),i=1,len(uline))
   write(output_unit,*) uline

end program multi_line
