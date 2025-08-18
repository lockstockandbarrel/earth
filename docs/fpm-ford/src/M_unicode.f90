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
      type(unicode_type)                     :: new
   end function new_string

   module function new_codes(codes) result(new)
      integer, intent(in)                    :: codes(:)
      type(unicode_type)                     :: new
   end function new_codes

end interface unicode_type        

! Assign a character sequence to a string.
interface assignment(=)
   module procedure :: assign_string_char
   module procedure :: assign_string_codes
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

interface range
   module procedure :: string_range
end interface range
public :: range

interface repeat
   module procedure :: repeat_string
end interface repeat
public :: repeat

! Returns length of character sequence without trailing spaces represented by the string.
!
! This method is elemental and returns a default integer scalar value.
interface len_trim
   module procedure :: len_trim_string
end interface len_trim
public :: len_trim

interface trim
   module procedure :: trim_string
end interface trim
public :: trim

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
! Constructor for new string instance from a vector integer value.
module function new_codes(codes) result(new)
integer,intent(in) :: codes(:)
type(unicode_type) :: new
   new%codes=codes
end function new_codes
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
! Assign a character sequence to a string.
subroutine assign_string_codes(lhs, rhs)
type(unicode_type), intent(inout) :: lhs
integer, intent(in)               :: rhs(:)
integer                           :: nerr
   lhs%codes=rhs
end subroutine assign_string_codes
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
! Return the character sequence represented by the string.
pure function string_range(string, first, last) result(uline)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: first
integer, intent(in)            :: last
type(unicode_type)             :: uline
integer                        :: nerr

   uline%codes = string%codes(first:last)

end function string_range
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Repeats the character sequence held by the string by the number of specified copies.
! This method is elemental and returns a scalar character value.
elemental function repeat_string(string, ncopies) result(repeated_string)
type(unicode_type), intent(in) :: string
integer, intent(in)            :: ncopies
type(unicode_type)             :: repeated_string
integer                        :: i

   repeated_string%codes=[(string%codes,i=1,ncopies)]

end function repeat_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Returns length of character sequence without trailing spaces represented by the string.
!
! space U+0020 32 Common Basic Latin Separator, Most common (normal
! ASCII space)
! 
! no-break space U+00A0 160 Common Latin-1 Supplement Separator,
! Non-breaking space: identical to U+0020, but not a point at which a line
! may be broken.
! 
! en quad U+2000 8192 General Punctuation Separator, Width of one en. U+2002
! is canonically equivalent to this character; U+2002 is preferred.
! 
! em quad U+2001 8193   Common General Punctuation Separator,
! Also known as "mutton quad". Width of one em. U+2003 is
! canonically equivalent to this character; U+2003 is preferred.
! 
! en space U+2002 8194   Common General Punctuation Separator,
! space Also known as "nut". Width of one en. U+2000 En Quad is
! canonically equivalent to this character; U+2002 is preferred.
! 
! em space U+2003 8195  Common General Punctuation Separator,
! space Also known as "mutton". Width of one em. U+2001 Em Quad is
! canonically equivalent to this character; U+2003 is preferred.
! 
! three-per-em space U+2004 8196 Common General Punctuation Separator,
! Also known as "thick space". One third of an em wide.
! 
! four-per-em space U+2005 8197 Common General Punctuation Separator,
! space Also known as "mid space". One fourth of an em wide.
! 
! six-per-em space U+2006 8198 Common General Punctuation Separator,
! space One sixth of an em wide. In computer typography, sometimes equated
! to U+2009.
! 
! figure space U+2007 8199 Common General Punctuation Separator, In fonts
! with monospaced digits, equal to the width of one digit.
! 
! punctuation space U+2008 8200 Common General Punctuation Separator,
! As wide as the narrow punctuation in a font, i.e. the advance width of
! the period or comma.
! 
! thin space U+2009 8201 Common General Punctuation Separator, one-fifth
! (sometimes one-sixth) of an em wide.  Recommended for use as a thousands
! separator for measures made with SI units. Unlike U+2002 to U+2008,
! its width may get adjusted in typesetting.
! 
! hair space U+200A 8202 Common General Punctuation Separator, space
! Thinner than a thin space.
! 
! narrow no-break space U+202F 8239 Common General Punctuation Separator,
! Similar in function to U+00A0
! 
! No-Break Space. When used with Mongolian, its width is usually one third
! of the normal space; in other context, its width sometimes resembles
! that of the Thin Space (U+2009).
! 
! medium mathematical space U+205F 8287   Common General Punctuation
! Separator, space MMSP. Used in mathematical formulae. Four-eighteenths
! of an em. In mathematical typography, the widths of spaces are usually
! given in integral multiples of an eighteenth of an em, and 4/18 em
! may be used in several situations, for example between the a and the +
! and between the + and the b in the expression a + b.
! 
! ideographic space U+3000 12288 　 Yes No Common CJK Symbols and
! Punctuation Separator, As wide as a CJK character cell (fullwidth). Used,
! for example, in tai tou.

elemental function len_trim_string(string) result(length)
type(unicode_type), intent(in) :: string
integer                        :: length

   do length=size(string%codes),1,-1
      if(any(string%codes(length).eq.[ 32,160,8192,8193,8194,8195,8196,8197,8198,8199,8200,8201,8202,8239,8287,12288 ]))cycle
      exit
   enddo

end function len_trim_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! This method is elemental and returns a scalar character value.
elemental function trim_string(string) result(trimmed_string)
type(unicode_type), intent(in) :: string
type(unicode_type)             :: trimmed_string
integer                        :: last

   last=len_trim_string(string)
   trimmed_string%codes=string%codes(:last)

end function trim_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_unicode
