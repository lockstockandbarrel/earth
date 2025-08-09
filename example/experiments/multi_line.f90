program multi_line
use iso_fortran_env, only : output_unit
use M_utf8, only : utf8_to_ucs4
implicit none
integer,parameter            :: ascii = selected_char_kind ("ascii")
integer,parameter            :: ucs4 = selected_char_kind("ISO_10646")
integer                      :: i, j, longest
character(len=3,kind=ucs4)   :: line
character(len=255),parameter :: upagain(3)=[character(len=255) :: &
"七転び八起き。", &
"転んでもまた立ち上がる。", &
"くじけずに前を向いて歩いていこう。"]
!
! Romanization:
! Nanakorobi yaoki. Koronde mo mata tachiagaru. Kujikezu ni mae o muite aruite ikou.
! or English translation
! "Fall seven times, stand up eight. Even if you fall down, you will get up again. Don't be discouraged, just keep walking forward."
!
character(len=4*len(upagain),kind=ucs4) :: ustr(size(upagain))

   open (output_unit, encoding='UTF-8')
! convert pseudo-utf-8 ascii to actual Fortran ucs4 unicode
   longest=0 ! get longest trimmed line
   do i=1,size(upagain)
      ustr(i)=utf8_to_ucs4(upagain(i))
      longest=max(longest,len_trim(ustr(i)))
   enddo
   write(output_unit,*)
   write(output_unit,*)'longest=',longest
   write(output_unit,*)(trim(ustr(i)),i=1,size(ustr))
   write(output_unit,*)
   ! does not work too well if not a fixed-space font, but even when fixed-spaced having problems in mintty anyway, at least
   do i=1,longest
      ! not lining up
      write(output_unit,'(*(A))') (ustr(j)(i:i),j=size(upagain),1,-1)
      ! numbers for T seems to be for ASCII bytes
      !write(output_unit,'(T1,A,T5,A,T9,A)') (ustr(j)(i:i),j=1,size(upagain))
      !line=ustr(3)(i:i)//ustr(2)(i:i)//ustr(1)(i:i)
      !write(*,*)line
   enddo
   do i=1,longest
      ! not lining up
      write(output_unit,'(*(A))') (ustr(j)(i:i),j=1,size(upagain))
   enddo
end program multi_line
