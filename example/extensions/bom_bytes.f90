program bom_exe
!
! create a Fortran source file starting with a utf-8 BOM to see if your
! compiler will compile it or fail because a character is not in the
! Fortran character set outside of a comment or literal string
!
use iso_fortran_env, only : stdout => output_unit
implicit none
character(len=*),parameter :: &
   & A_bom = char(int(z'EF'))// char(int(z'BB'))// char(int(z'BF'))

   write(stdout,'(a)',advance='no')A_bom
   write(stdout,'(a)') &
    'program testit ! Unicode BOM as utf-8 bytes'               ,&
    '   write(*,*)"File starts with BOM from ""bytes"" write!"' ,&
    'end program testit'

end program bom_exe
