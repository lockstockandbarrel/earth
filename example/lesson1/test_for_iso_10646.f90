   program test_for_iso_10646
   use iso_fortran_env, only : output_unit
   implicit none
   intrinsic selected_char_kind
   integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')
      open(output_unit,encoding='utf-8')
      write(*,*)'Smiling face with open mouth',char(int(z'1F603'),kind=ucs4) ! 😃
   end program test_for_iso_10646
