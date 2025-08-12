## Introduction to Fortran Unicode support
### Lesson III: mixing ASCII and UCS4 kinds as regards assignments,
### concatenation, passing arguments to external ASCII libraries, and I/O argument lists

** WIP!**

```fortran
program ascii_and_ucs4
use iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none

intrinsic selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')

character(len=:),allocatable           :: aline
character(len=:,kind=ucs4),allocatable :: uline
character(len=1),allocatable           :: ch(:), ch2(:)
character(len=1,kind=ucs4),allocatable :: glyph(:)
integer                                :: i
integer                                :: iostat
integer                                :: nerr
character(len=1)                       :: paws

   open (stdout, encoding='DEFAULT')
   open (stdout, encoding='UTF-8')

   ! cannot concatenate character variables of different kinds
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

   write(stdout,'(A)')'round trip for all ASCII bytes'

   write(stdout,'(A)')repeat(ucs4_'=',80)
   ch=[(char(i),i=0,255)]
   open (stdout, encoding='DEFAULT')
   write(stdout,'(10(g0,1x,g0,1x))')(ch(i),i=0,255)
   open (stdout, encoding='UTF-8')
   write(stdout,'(10(g0,1x,g0,1x))')(ch(i),i=0,255)
   read(stdin,'(a)',iostat=iostat)paws

   write(stdout,'(A)')repeat(ucs4_'=',80)
   glyph=ch
   write(stdout,'(10(g0,1x,g0,1x))')(glyph(i),i=0,255)
   read(stdin,'(a)',iostat=iostat)paws

   write(stdout,'(A)')repeat(ucs4_'=',80)
   ch2=glyph
   write(stdout,'(10(g0,1x,g0,1x))')(ch2(i),i=0,255)
   read(stdin,'(a)',iostat=iostat)paws

   write(stdout,'(A)')repeat(ucs4_'=',80)

   write(stdout,'(a,L0)') 'roundrobin returned all values unchanged?',all( ch .eq. ch2)
      
end program ascii_and_ucs4
```
+ [PREVIOUS](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson2_ucs4.md)
+ [NEXT](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson4_ucs4.md)
