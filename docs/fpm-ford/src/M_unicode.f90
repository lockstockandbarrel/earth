module M_unicode
! Unicode-related procedures not requiring compiler support of ISO-10646
! first presented in https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949
! including enhancements and latin support from Francois Jacq, 2025-08

implicit none

private
public  :: utf8_to_codepoints,  codepoints_to_utf8
public  :: isolatin_to_unicode, unicode_to_isolatin
public  :: utf8_to_isolatin,    isolatin_to_utf8

private :: a2s, s2a

public :: unicode_type
public :: assignment(=)

interface utf8_to_codepoints
   module procedure utf8_to_codepoints_string,utf8_to_codepoints_chars
end interface utf8_to_codepoints

interface codepoints_to_utf8
   module procedure codepoints_to_utf8_string,codepoints_to_utf8_chars
end interface codepoints_to_utf8

type :: unicode_type ! Unicode string type holding an arbitrary sequence of integer codes.
   sequence ! not used for storage association; a kludge to prevent extending this type. 
   private
   integer, allocatable :: codes(:)
end type unicode_type

! Constructor for new string instances
interface unicode_type
   elemental module function new_string(string) result(new)
      character(len=*), intent(in), optional :: string
      type(unicode_type) :: new
   end function new_string

end interface unicode_type        

! Assign a character sequence to a string.
interface assignment(=)
   module procedure :: assign_string_char
end interface assignment(=)

!> Return length of the character sequence in glyphs 
!> This method is elemental as each string element can have a different length ; returns a default integer scalar value.
interface len
   module procedure :: len_string
end interface len
public :: len

interface character
   module procedure :: char_string
   module procedure :: char_string_range
end interface character
public :: character

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine codepoints_to_utf8_chars(unicode,utf8,nerr)

integer,intent(in)                :: unicode(:)
character,allocatable,intent(out) :: utf8(:)
integer,intent(out)               :: nerr
integer                           :: i, n_unicode, n_utf8, cp
character, allocatable            :: temp_utf8(:)

   nerr=0

   n_unicode = size(unicode)

   allocate(temp_utf8(4*n_unicode))
   n_utf8 = 0

   do i = 1, n_unicode
      cp = unicode(i)

      select case (cp)
      case (0:127) ! 1 byte : 0xxxxxxx
         n_utf8 = n_utf8 + 1
         temp_utf8(n_utf8) = achar(cp)

      case (128:2047) ! 2 bytes : 110xxxxx 10xxxxxx
         n_utf8 = n_utf8 + 2
         temp_utf8(n_utf8-1) = achar(ior(192, ishft(cp, -6)))
         temp_utf8(n_utf8)   = achar(ior(128, iand(cp, 63)))

      case (2048:65535) ! 3 bytes : 1110xxxx 10xxxxxx 10xxxxxx
         if (cp >= 55296 .and. cp <= 57343) then
            nerr=nerr+1
            n_utf8 = n_utf8 + 1
            temp_utf8(n_utf8) = '?'
            cycle
         endif
         n_utf8 = n_utf8 + 3
         temp_utf8(n_utf8-2) = achar(ior(224, ishft(cp, -12)))
         temp_utf8(n_utf8-1) = achar(ior(128, iand(ishft(cp, -6), 63)))
         temp_utf8(n_utf8)   = achar(ior(128, iand(cp, 63)))

      case (65536:1114111) ! 4 bytes : 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
         n_utf8 = n_utf8 + 4
         temp_utf8(n_utf8-3) = achar(ior(240, ishft(cp, -18)))
         temp_utf8(n_utf8-2) = achar(ior(128, iand(ishft(cp, -12), 63)))
         temp_utf8(n_utf8-1) = achar(ior(128, iand(ishft(cp, -6), 63)))
         temp_utf8(n_utf8)   = achar(ior(128, iand(cp, 63)))

      case default
         nerr=nerr+1
         n_utf8 = n_utf8 + 1
         temp_utf8(n_utf8) = '?'
      end select
   enddo

   allocate(utf8(n_utf8))
   utf8 = temp_utf8(1:n_utf8)

end subroutine codepoints_to_utf8_chars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_codepoints_chars(utf8,unicode,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character            ,intent(in)  :: utf8(:)
integer  ,allocatable,intent(out) :: unicode(:)
integer,intent(out)               :: nerr
integer                           :: n_out
integer                           :: i, len8, b1, b2, b3, b4
integer                           :: cp, nbytes,nerr0
integer,allocatable               :: temp(:)

   nerr = 0

   len8 = size(utf8)
   i = 1
   n_out = 0
   allocate(temp(len8)) ! big enough to store all unicode values

   do while (i <= len8)

      nerr0=nerr

      b1 = iachar(utf8(i))
      if (b1 < 0) b1 = b1 + 256

      nbytes = 1

      select case (b1)

      case (0:127)
         cp = b1

      case (192:223)
         if (i+1 > len8) then
            nbytes=len8-i+1
            nerr = nerr+1
            cp=IACHAR('?')
         else
            nbytes=2
            b2 = iachar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            if (iand(b2, 192) /= 128) then
               nerr=nerr+1
               cp=IACHAR('?')
            else
               cp = iand(b1, 31)
               cp = ishft(cp,6) + iand(b2,63)
            endif
         endif

      case (224:239)
         if (i+2 > len8) then
            nbytes=len8-i+1
            nerr=nerr+1
            cp=IACHAR('?')
         else
            nbytes = 3
            b2 = iachar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            b3 = iachar(utf8(i+2)); if (b3 < 0) b3 = b3 + 256
            if (iand(b2, 192) /= 128 .or. iand(b3, 192) /= 128) then
               nerr =nerr+1
               cp=IACHAR('?')
            else
               cp = iand(b1, 15)
               cp = ishft(cp,6) + iand(b2,63)
               cp = ishft(cp,6) + iand(b3,63)
            endif
         endif

      case (240:247)
         if (i+3 > len8) then
            nbytes=len8-i+1
            nerr = nerr+1
            cp=IACHAR('?')
         else
            nbytes = 4
            b2 = iachar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            b3 = iachar(utf8(i+2)); if (b3 < 0) b3 = b3 + 256
            b4 = iachar(utf8(i+3)); if (b4 < 0) b4 = b4 + 256
            if (iand(b2,192)/=128 .or. iand(b3,192)/=128 .or. iand(b4,192)/=128) then
               nerr = nerr+1
               cp=IACHAR('?')
            else
               cp = iand(b1, 7)
               cp = ishft(cp,6) + iand(b2,63)
               cp = ishft(cp,6) + iand(b3,63)
               cp = ishft(cp,6) + iand(b4,63)
            endif
         endif

      case default
         nerr=nerr+1
         cp=IACHAR('?')

      end select

      if(nerr0 /= nerr) then
         ! This is an invalid UTF-8 start byte. We apply the heuristic
         ! and interpret it as an ISO-8859-15 character.
         select case (b1)
         case (164); cp = 8364 ! Euro
         case (166); cp = 352  ! S caron
         case (168); cp = 353  ! s caron
         case (180); cp = 381  ! Z caron
         case (184); cp = 382  ! z caron
         case (188); cp = 338  ! OE
         case (189); cp = 339  ! oe
         case (190); cp = 376  ! Y trema
         case default
            cp = b1 ! For all other chars, the codepoint is the byte value
         end select
         nbytes=1
      endif

      n_out = n_out + 1
      temp(n_out) = cp
      i = i + nbytes

   enddo

   allocate(unicode(n_out))
   unicode = temp(1:n_out)

end subroutine utf8_to_codepoints_chars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine isolatin_to_unicode(isolatin,unicode,nerr)
character           ,intent(in)  :: isolatin(:)
integer, allocatable,intent(out) :: unicode(:)
integer             ,intent(out) :: nerr

integer :: i, n, char_code

   nerr = 0
   n = size(isolatin)
   allocate(unicode(n))

   do i = 1, n
      char_code = iachar(isolatin(i))
      ! Only 8 characters do not correspond to unicode
      select case (char_code)
      case (164); unicode(i) = 8364 ! Symbol Euro
      case (166); unicode(i) = 352  ! S caron
      case (168); unicode(i) = 353  ! s caron
      case (180); unicode(i) = 381  ! Z caron
      case (184); unicode(i) = 382  ! z caron
      case (188); unicode(i) = 338  ! OE majuscule
      case (189); unicode(i) = 339  ! oe minuscule
      case (190); unicode(i) = 376  ! Y trema
      case default
         unicode(i) = char_code
      end select
   enddo

end subroutine isolatin_to_unicode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine unicode_to_isolatin(unicode,isolatin,nerr)

integer              ,intent(in)  :: unicode(:)
character,allocatable,intent(out) :: isolatin(:)
integer              ,intent(out) :: nerr

integer :: i, n, cp
integer :: replacement_count

   nerr=0
   n = size(unicode)
   allocate(isolatin(n))

   do i = 1, n
      cp = unicode(i)
      select case (cp) ! 8 special characters
      case (8364); isolatin(i) = achar(164) ! Euro
      case (352);  isolatin(i) = achar(166) ! S caron
      case (353);  isolatin(i) = achar(168) ! s caron
      case (381);  isolatin(i) = achar(180) ! Z caron
      case (382);  isolatin(i) = achar(184) ! z caron
      case (338);  isolatin(i) = achar(188) ! OE majuscule
      case (339);  isolatin(i) = achar(189) ! oe minuscule
      case (376);  isolatin(i) = achar(190) ! Y trema
      case (0:163, 165, 167, 169:179, 181:183, 185:187, 191:255)
         isolatin(i) = achar(cp)
      case default
         nerr=nerr+1
         isolatin(i) = '?' ! replacement character
      end select
   enddo

end subroutine unicode_to_isolatin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine isolatin_to_utf8(isolatin,utf8,nerr)
character            ,intent(in)  :: isolatin(:)
character,allocatable,intent(out) :: utf8(:)
integer              ,intent(out) :: nerr
integer,allocatable :: unicode(:)

   call isolatin_to_unicode(isolatin,unicode,nerr)
   call codepoints_to_utf8(unicode,utf8,nerr)

end subroutine isolatin_to_utf8
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_isolatin(utf8,isolatin,nerr)
character            ,intent(in)  :: utf8(:)
character,allocatable,intent(out) :: isolatin(:)
integer              ,intent(out) :: nerr
integer,allocatable :: unicode(:)

   call utf8_to_codepoints(utf8,unicode,nerr)
   call unicode_to_isolatin(unicode,isolatin,nerr)

end subroutine utf8_to_isolatin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function a2s(array)  result (string)

! @(#) M_strings a2s(3fp) function to copy char array to string

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i

   forall( i = 1:size(array)) string(i:i) = array(i)
!  string=transfer(array,string)

end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

! @(#) M_strings s2a(3fp) function to copy string(1 Clen(string)) to char array

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i

   forall(i=1:len(string)) array(i) = string(i:i)
!  array=transfer(string,array)

end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine codepoints_to_utf8_string(unicode,utf8,nerr)

integer,intent(in)                       :: unicode(:)
character(len=:),allocatable,intent(out) :: utf8
integer,intent(out)                      :: nerr
character, allocatable                   :: utf8_chars(:)
   call codepoints_to_utf8_chars(unicode,utf8_chars,nerr)
   utf8=a2s(utf8_chars)
end subroutine codepoints_to_utf8_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_codepoints_string(utf8,unicode,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character(len=*),intent(in)     :: utf8
integer,allocatable,intent(out) :: unicode(:)
integer,intent(out)             :: nerr
character,allocatable           :: temp(:)
   temp=s2a(utf8)
   call utf8_to_codepoints_chars(temp,unicode,nerr)
end subroutine utf8_to_codepoints_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Constructor for new string instances from a scalar character value.
elemental module function new_string(string) result(new)
character(len=*), intent(in), optional :: string
type(unicode_type)                     :: new
integer                                :: nerr
   if (present(string)) then
      call utf8_to_codepoints_string(string,new%codes,nerr)
   endif
end function new_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Assign a character sequence to a string.
elemental subroutine assign_string_char(lhs, rhs)
type(unicode_type), intent(inout) :: lhs
character(len=*), intent(in)      :: rhs
integer                           :: nerr
   call utf8_to_codepoints_string(rhs,lhs%codes,nerr)
end subroutine assign_string_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

! Returns the length of the character sequence represented by the string.
elemental function len_string(string) result(length)
type(unicode_type), intent(in) :: string
integer :: length

   if (allocated(string%codes)) then
      length = size(string%codes)
   else
      length = 0
   endif

end function len_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Return the character sequence represented by the string.
pure function char_string(string) result(aline)
type(unicode_type), intent(in) :: string
character(len=:),allocatable   :: aline
integer                        :: nerr

call codepoints_to_utf8_string(string%codes,aline,nerr)

end function char_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Return the character sequence represented by the string.
pure function char_string_range(string, first, last) result(aline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: first
integer, intent(in)            :: last
!character(len=last-first+1)    :: aline
character(len=:),allocatable   :: aline
integer                        :: nerr

   call codepoints_to_utf8_string(string%codes(first:last),aline,nerr)

end function char_string_range
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_unicode
