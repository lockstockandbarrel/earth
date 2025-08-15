program bom_exe
!
! create a Fortran source file starting with a utf-8 BOM to see if your
! compiler will compile it or fail because a character is not in the
! Fortran character set outside of a comment or literal string
!
use iso_fortran_env, only : stdout => output_unit
implicit none
intrinsic selected_char_kind
integer,parameter :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*,kind=ucs4),parameter :: U_bom=char(int(z'FEFF'),kind=ucs4)

   open(stdout,encoding='UTF-8')
   write(stdout,'(a)',advance='no')U_bom
   write(stdout,'(a)') &

    ucs4_'program testit ! Unicode BOM encoded to utf-8 bytes by Fortran' ,&
    ucs4_'   write(*,*)"File starts with BOM from UCS-4 write!"'          ,&
    ucs4_'end program testit'

end program bom_exe
