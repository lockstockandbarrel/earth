program note
use iso_fortran_env, only : output_unit
implicit none
intrinsic selected_char_kind
integer,parameter          :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*,kind=ucs4),parameter :: pointer = char(int(z'1FBC1'),kind=ucs4)// &
                                                  char(int(z'1FBC2'),kind=ucs4)// &
                                                  char(int(z'1FBC3'),kind=ucs4)
   open(output_unit,encoding='UTF-8')
   write(*,*)pointer
end program note
