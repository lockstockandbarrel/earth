program concatenate
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
   aline=uline ! conversion by assignment
   write(stdout,'(a)') aline
   aline=ucs4_to_ascii(uline) ! is "smiley" replaced with a character used for errors?
   write(stdout,'(a)') aline
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
