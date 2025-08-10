program ascii_and_ucs4
use iso_fortran_env, only : stdout=>output_unit
implicit none

intrinsic selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')

character(len=:),allocatable           :: aline
character(len=:,kind=ucs4),allocatable :: uline
character(len=1),allocatable           :: ch(:)
character(len=1,kind=ucs4),allocatable :: glyph(:)
integer                                :: i
integer                                :: nerr

   open (stdout, encoding='DEFAULT')
   open (stdout, encoding='UTF-8')

   ! cannot cocatenate character variables of different kinds
   !uline='ascii string' // ucs4_'unicode string'

   ! only characters defined in the other encoding are copied on an assign

   write(stdout,'(A)')repeat(' ',80)
   write(stdout,'(A)')'assign RHS ucs4 to LHS ascii'
   uline=char(int(z'261B'),ucs4) // ucs4_'UCS-4 string' // char(int(z'261A'),ucs4)
   write(stdout,'(a)')trim(uline)
   aline=uline ! only the ASCII 7-bit characters are copied
   write(stdout,'(a)')trim(aline) // ' assigned to ASCII'

   write(stdout,'(A)')repeat(' ',80)
   write(stdout,'(A)')'assign LHS ascii to RHS ucs4'
   aline=ascii_'ASCII string' 
   write(stdout,'(a)')trim(aline)
   uline=aline ! all ASCII 7-bit characters can be represented in UCS-4
   write(stdout,'(a)')trim(uline)//ucs4_' assigned to UCS4'
      
end program ascii_and_ucs4
