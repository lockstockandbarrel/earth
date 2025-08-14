## Introduction to Fortran Unicode support
### Lesson III: mixing ASCII and UCS4 kinds as regards concatenation, assignments,
### passing arguments to external ASCII libraries, and I/O argument lists

** WIP!**

### Concatenation, Assignment, and automatic conversion

Concerning assignment -- the Fortran standard states

   if the variable is of type character and of ISO 10646, ASCII, or default
   character kind, expr shall be of ISO 10646, ASCII, or default character
   kind, otherwise if the variable is of type character expr shall have
   the same kind type parameter,
   
   For an intrinsic assignment statement where the variable is of type
   character, if expr has a different kind type parameter, each character c
   in expr is converted to the kind type parameter of the variable by 

       ACHAR(IACHAR(c),KIND(variable)).

   NOTES

   For nondefault character kinds, the blank padding character is
   processor dependent. 
   
   When assigning a character expression to a variable of a different kind,
   each character of the expression that is not representable in the kind
   of the variable is replaced by a processor-dependent character.

### Concatenation
```fortran
program concatenate
use iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none

intrinsic selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')

character(len=*),parameter             :: g='(*(g0))'
character(len=:),allocatable           :: aline, a1, a2
character(len=:,kind=ucs4),allocatable :: uline, u1, u2
character(len=1),allocatable           :: ch(:), ch2(:)
character(len=1,kind=ucs4),allocatable :: glyph(:)
integer                                :: i
integer                                :: iostat
integer                                :: nerr
character(len=1)                       :: paws
character(len=1,kind=ucs4)             :: smiley=char(int(z'1F603'),kind=ucs4) ! 😃 Smiling face with open mouth

   open (stdout, encoding='DEFAULT')
   open (stdout, encoding='UTF-8')
   !
   ! Concatenation:
   !
   write(stdout,'(A)')repeat('=',80)
   write(stdout,'(a)')'strings of different kinds cannot be concatenated.'
   !uline='ascii string'// smiley // 'ascii string' ! NO. Kinds must match

   write(stdout,'(a)') 'Of course constants can have their KIND specified.'
   uline=ucs4_'first UCS4 string' // smiley // ucs4_'another UCS4 string '
   write(stdout,'(A)') uline
   !
   write(stdout,'(A)')repeat('=',80)
   write(stdout,'(a)') 'you can use simple assigns to do conversions'
   ! so if I have a UCS4 string
   u1=smiley // ucs4_'UCS4 strings' // smiley // ucs4_'appended together' // smiley
   ! and an ASCII string
   a1='ascii strings' // 'appended together'
   ! the ASCII string can be converted to UCS4 with an assign
   u2=a1 ! use allocation to convert ASCII to UCS4
   ! now with a copy of everthing as UCS4 the append will work
   uline=u1//u2 ! now append together the two strings which are now of the same kind
   write(stdout,'(a)') uline
   ! 
   write(stdout,'(A)')repeat('=',80)
   write(stdout,'(a)') 'we can make functions to convert to and from ASCII and UCS4'
   ! using the same conversions as used by an assign.
   uline=smiley // ascii_to_ucs4('ascii string') // smiley // ucs4_'unicode string' // smiley
   write(stdout,'(a)') uline
   !
   write(stdout,'(A)')'unrepresentable characters:'
   write(stdout,'(a)')'what about characters that have no equivalent in the other kind?'
   write(stdout,'(A)')'conversion by assignment'
   aline=uline 
   write(stdout,g) aline,' ',len(aline),' ',len(uline)
   write(stdout,'(a)') 'conversion by ACHAR/ICHAR:'
   aline=ucs4_to_ascii(uline) ! is "smiley" replaced with a character used for errors?
   write(stdout,g) aline,' ',len(aline),' ',len(uline)
   write(stdout,'(a)') 'which character replaces the unrepresentable characters is processor-dependent'
   write(stdout,'(a)') 'and might be unprintable'
   aline=smiley
   write(stdout,'(a,i0,a)') 'ADE:',ichar(aline),' CHARACTER:',aline
   write(stdout,'(A)')repeat('=',80)

contains

function ascii_to_ucs4(astr) result(ustr)
! @(#) make the same conversion as an assignment statement from ASCII to UCS4
character(len=*,kind=ascii),intent(in) :: astr
character(len=len(astr),kind=ucs4)     :: ustr
integer                                :: i
   do i=1,len(astr)
      ustr(i:i)=achar(iachar(astr(i:i)),kind=ucs4)
   enddo
end function ascii_to_ucs4

function ucs4_to_ascii(ustr) result(astr)
! @(#) make the same conversion as an assignment statement from UCS4 o ASCII
character(len=*,kind=ucs4),intent(in)  :: ustr
character(len=len(ustr),kind=ascii)    :: astr
integer                                :: i
   do i=1,len(ustr)
      astr(i:i)=achar(iachar(ustr(i:i)),kind=ascii)
   enddo
end function ucs4_to_ascii

end program concatenate
```
### Assignment
```fortran
program assignment
use iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none

intrinsic selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')

character(len=:),allocatable           :: aline, a1, a2
character(len=:,kind=ucs4),allocatable :: uline, u1, u2
character(len=1),allocatable           :: ch(:), ch2(:)
character(len=1,kind=ucs4),allocatable :: glyph(:)
integer                                :: i
integer                                :: iostat
integer                                :: nerr
character(len=1)                       :: paws
character(len=1,kind=ucs4)             :: smiley=char(int(z'1F603'),kind=ucs4) ! 😃 Smiling face with open mouth

   open (stdout, encoding='DEFAULT')
   open (stdout, encoding='UTF-8')
   !
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

end program assignment
```
+ [PREVIOUS](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson2_ucs4.md)
+ [NEXT](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson4_ucs4.md)
