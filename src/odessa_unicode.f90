module odessa_unicode
! An enhanced version from Francois Jacq, 2025-08
! presented in https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949

implicit none

private
public :: u32 ! character type for UTF-32 strings
public :: utf8_to_unicode
public :: unicode_to_utf8
public :: utf8_to_utf32
public :: utf32_to_utf8

public :: isolatin_to_unicode
public :: unicode_to_isolatin
public :: utf8_to_isolatin
public :: isolatin_to_utf8
public :: isolatin_to_utf32
public :: utf32_to_isolatin

integer,parameter :: u32 = selected_char_kind ('ISO_10646')

contains

subroutine unicode_to_utf8(unicode,utf8,nerr)

integer  ,intent(in)              :: unicode(:)
character,allocatable,intent(out) :: utf8(:)
integer              ,intent(out) :: nerr

integer :: i, n_unicode, n_utf8, cp
character, allocatable :: temp_utf8(:)

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

end subroutine unicode_to_utf8

subroutine utf8_to_unicode(utf8,unicode,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character            ,intent(in)  :: utf8(:)
integer  ,allocatable,intent(out) :: unicode(:)
integer              ,intent(out) :: nerr

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
         case (164); cp = 8364  ! Euro
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

end subroutine utf8_to_unicode

subroutine utf8_to_utf32(utf8,utf32,nerr)
character                      ,intent(in)  :: utf8(:)
character(kind=u32),allocatable,intent(out) :: utf32(:)
integer                        ,intent(out) :: nerr
integer            ,allocatable             :: unicode(:)
integer                                     :: i, n

   call utf8_to_unicode(utf8,unicode,nerr)
   n=size(unicode)
   allocate(utf32(n))
   do i=1,n
      utf32(i)=char(unicode(i),kind=u32)
   enddo

end subroutine utf8_to_utf32


subroutine utf32_to_utf8(utf32,utf8,nerr)
character(kind=u32)            ,intent(in)  :: utf32(:)
character          ,allocatable,intent(out) :: utf8(:)
integer                        ,intent(out) :: nerr
integer            ,allocatable             :: unicode(:)
integer                                     :: i, n

   n=size(utf32)
   allocate(unicode(n))
   do i=1,n
      unicode(i)=ICHAR(utf32(i))
   enddo
   call unicode_to_utf8(unicode,utf8,nerr)

end subroutine

subroutine isolatin_to_unicode(isolatin,unicode,nerr)
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
      case (164) ! Symbol Euro
         unicode(i) = 8364
      case (166) ! S caron
         unicode(i) = 352
      case (168) ! s caron
         unicode(i) = 353
      case (180) ! Z caron
         unicode(i) = 381
      case (184) ! z caron
         unicode(i) = 382
      case (188) ! OE majuscule
         unicode(i) = 338
      case (189) ! oe minuscule
         unicode(i) = 339
      case (190) ! Y trema
         unicode(i) = 376
      case default
         unicode(i) = char_code
      end select
   enddo

end subroutine

subroutine unicode_to_isolatin(unicode,isolatin,nerr)

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
      select case (cp)
      ! 8 special characters
      case (8364) ! Euro
         isolatin(i) = achar(164)
      case (352)  ! S caron
         isolatin(i) = achar(166)
      case (353)  ! s caron
         isolatin(i) = achar(168)
      case (381)  ! Z caron
         isolatin(i) = achar(180)
      case (382)  ! z caron
         isolatin(i) = achar(184)
      case (338)  ! OE majuscule
         isolatin(i) = achar(188)
      case (339)  ! oe minuscule
         isolatin(i) = achar(189)
      case (376)  ! Y trema
         isolatin(i) = achar(190)
      case (0:163, 165, 167, 169:179, 181:183, 185:187, 191:255)
         isolatin(i) = achar(cp)
      case default
         nerr=nerr+1
         isolatin(i) = '?' ! replacement character
      end select
   enddo

end subroutine

subroutine isolatin_to_utf8(isolatin,utf8,nerr)
character            ,intent(in)  :: isolatin(:)
character,allocatable,intent(out) :: utf8(:)
integer              ,intent(out) :: nerr
integer,allocatable :: unicode(:)

   call isolatin_to_unicode(isolatin,unicode,nerr)
   call unicode_to_utf8(unicode,utf8,nerr)

end subroutine

subroutine utf8_to_isolatin(utf8,isolatin,nerr)
character            ,intent(in)  :: utf8(:)
character,allocatable,intent(out) :: isolatin(:)
integer              ,intent(out) :: nerr
integer,allocatable :: unicode(:)

   call utf8_to_unicode(utf8,unicode,nerr)
   call unicode_to_isolatin(unicode,isolatin,nerr)

end subroutine

subroutine isolatin_to_utf32(isolatin,utf32,nerr)
character                      ,intent(in)  :: isolatin(:)
character(kind=u32),allocatable,intent(out) :: utf32(:)
integer                        ,intent(out) :: nerr
integer            ,allocatable             :: unicode(:)
integer                                     :: i, n

   call isolatin_to_unicode(isolatin,unicode,nerr)
   n=size(unicode)
   allocate(utf32(n))
   do i=1,n
      utf32(i)=char(unicode(i),kind=u32)
   enddo

end subroutine

subroutine utf32_to_isolatin(utf32,isolatin,nerr)
character(kind=u32)            ,intent(in)  :: utf32(:)
character          ,allocatable,intent(out) :: isolatin(:)
integer                        ,intent(out) :: nerr
integer            ,allocatable             :: unicode(:)
integer                                     :: i, n

   n=size(utf32)
   allocate(unicode(n))
   do i=1,n
      unicode(i)=ICHAR(utf32(i))
   enddo
   call unicode_to_isolatin(unicode,isolatin,nerr)

end subroutine

end module
