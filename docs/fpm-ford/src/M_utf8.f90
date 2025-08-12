module M_utf8
use iso_fortran_env, only: error_unit, stderr=>error_unit
implicit none

private
public :: utf8_to_codepoints,  codepoints_to_utf8
public :: utf8_to_ucs4,        ucs4_to_utf8
public :: utf8_to_ucs4_via_io, ucs4_to_utf8_via_io
public :: ascii_to_ucs4,       ucs4_to_ascii

integer, parameter :: ucs4 = selected_char_kind('ISO_10646') ! The compiler must support UCS-4 characters
integer, parameter :: ascii = selected_char_kind('ascii')    ! maybe should use default, as ASCII is technically 128, not 256 chars

contains

subroutine utf8_to_codepoints(utf8, out, err)
! convert utf-8 characters to unicode codepoints (ie. integers) without requiring compiler to support unicode
character(len=*), intent(in)      :: utf8
integer, allocatable, intent(out) :: out(:) ! Unicode code points
integer, intent(out)              :: err

integer                           :: n_out, len8
integer                           :: i, b1, b2, b3, b4
integer                           :: cp, nbytes
integer                           :: temp(len(utf8)) ! big enough to hold all of utf8 even if each byte is a glyph

   err = 0
   len8 = len_trim(utf8)
   i = 1
   n_out = 0

   do while (i <= len8)
      b1 = iachar(utf8(i:i))
      if (b1 < 0) b1 = b1 + 256

      select case (b1)
      case (0:127)
         cp = b1
         nbytes = 1
      case (192:223)
         if (i+1 > len8) then
            err = 1
            return
         else
            b2 = iachar(utf8(i+1:i+1)); if (b2 < 0) b2 = b2 + 256
            if (iand(b2, 192) /= 128) then; err = 2; return; endif
            cp = iand(b1, 31)
            cp = ishft(cp,6) + iand(b2,63)
            nbytes = 2
         endif
      case (224:239)
         if (i+2 > len8) then
            err = 1
            return
         else
            b2 = iachar(utf8(i+1:i+1)); if (b2 < 0) b2 = b2 + 256
            b3 = iachar(utf8(i+2:i+2)); if (b3 < 0) b3 = b3 + 256
            if (iand(b2, 192) /= 128 .or. iand(b3, 192) /= 128) then
               err = 2
               return
            else
               cp = iand(b1, 15)
               cp = ishft(cp,6) + iand(b2,63)
               cp = ishft(cp,6) + iand(b3,63)
               nbytes = 3
            endif
         endif
      case (240:247)
         if (i+3 > len8) then
            err = 1
            return
         else
            b2 = iachar(utf8(i+1:i+1)); if (b2 < 0) b2 = b2 + 256
            b3 = iachar(utf8(i+2:i+2)); if (b3 < 0) b3 = b3 + 256
            b4 = iachar(utf8(i+3:i+3)); if (b4 < 0) b4 = b4 + 256
            if (iand(b2,192)/=128 .or. iand(b3,192)/=128 .or. iand(b4,192)/=128) then
              err = 2
              return
            endif
            cp = iand(b1, 7)
            cp = ishft(cp,6) + iand(b2,63)
            cp = ishft(cp,6) + iand(b3,63)
            cp = ishft(cp,6) + iand(b4,63)
            nbytes = 4
         endif
      case default
         err = 3
         return
      end select

      if (n_out >= size(temp)) then
         err = 4
         return
      endif
      n_out = n_out + 1
      temp(n_out) = cp
      i = i + nbytes
   enddo

   allocate(out(n_out))
   out = temp(1:n_out)

end subroutine utf8_to_codepoints

subroutine codepoints_to_utf8(codepoints,string,err)
!
! converting Unicode code points to UTF-8
! internally without using I/O directly requires manual implementation of
! the UTF-8 encoding rules because there's no direct intrinsic function
! for this conversion.
!
!    UTF-8 is a variable-width encoding that uses 1 to 4 bytes to represent
!    Unicode characters.
!
!    The number of bytes depends on the value of the Unicode code point:
!        0x0000  - 0x007F (ASCII): 1 byte (0xxxxxxx).
!        0x0080  - 0x07FF: 2 bytes (110xxxxx 10xxxxxx).
!        0x0800  - 0xFFFF (Basic Multilingual Plane): 3 bytes (1110xxxx 10xxxxxx 10xxxxxx).
!        0x10000 - 0x10FFFF (Supplementary Planes): 4 bytes (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx).
!
!    Continuation bytes in multi-byte sequences always start with the
!    bit pattern 10xxxxxx.
!
! Conversion Algorithm (Conceptual Example)
!
! Iterate through the 4-byte codepoint integers, each of which identifies a unicode glyph.
! Apply the UTF-8 encoding rules based on the code point's value to
! build a new byte array or a character string with character(len=1) elements, representing the
! UTF-8 sequence.
!
! The Unicode standard doesn't require a BOM for UTF-8, but it may be
! encountered at the start of files. This internal conversion doesn't
! add a BOM.
!
! This procedure does not explicitly handle surrogate pairs, which are
! used in UTF-16 to represent characters outside the Basic Multilingual
! Plane. Since UCS-4 already directly represents code points, surrogates
! are not a direct concern for UCS-4 to UTF-8 conversion, but it's
! good to be aware of them when dealing with other Unicode encodings.
!
use iso_fortran_env, only: error_unit

integer,intent(in)                      :: codepoints(:)
integer, intent(out),optional           :: err
character(len=:,kind=ascii),allocatable :: string

! utf8_bytes stores the resulting UTF-8 byte sequence. It's declared
! with kind=ascii and a length determined by the maximum possible
! bytes for the UCS-4 string length (each UCS-4 character can be up
! to 4 bytes in UTF-8).
character(len=size(codepoints)*4)       :: utf8_bytes ! Max 4 bytes per UCS-4 char

integer                                 :: i, j, code_point
integer                                 :: num_bytes, utf8_index
integer, dimension(4)                   :: utf8_code

  utf8_index = 1
  if(present(err)) err = 0

  do i = 1, size(codepoints)    ! iterate through each UCS-4 character.

    code_point = codepoints(i)  ! Get the integer value of the UCS-4 char

    ! The if/elseif statements check the code point value and determine
    ! the number of bytes required for UTF-8 encoding.
    !
    ! Bitwise operations (ior, ishft, iand) are used to construct the
    ! individual UTF-8 bytes according to the standard rules.
    !
    if (code_point < int(z'80') ) then  ! 1-byte UTF-8 (ASCII)
      num_bytes = 1
      utf8_code(1) = code_point
    else if (code_point < int(z'800') ) then ! 2-byte UTF-8
      num_bytes = 2
      utf8_code(1) = ior(z'C0', ishft(code_point, -6))
      utf8_code(2) = ior(z'80', iand(code_point, z'3F'))
    else if (code_point < int(z'10000') ) then ! 3-byte UTF-8
      num_bytes = 3
      utf8_code(1) = ior(z'E0', ishft(code_point, -12))
      utf8_code(2) = ior(z'80', iand(ishft(code_point, -6), z'3F'))
      utf8_code(3) = ior(z'80', iand(code_point, z'3F'))
    else if (code_point < int(z'110000') ) then ! 4-byte UTF-8
      num_bytes = 4
      utf8_code(1) = ior(z'F0', ishft(code_point, -18))
      utf8_code(2) = ior(z'80', iand(ishft(code_point, -12), z'3F'))
      utf8_code(3) = ior(z'80', iand(ishft(code_point, -6), z'3F'))
      utf8_code(4) = ior(z'80', iand(code_point, z'3F'))
    else
      ! Handle invalid code points or error conditions as needed
      write(error_unit,*) "Error: Invalid Unicode code point:", code_point
      if(.not.present(err))stop 1
      err=err+1
      cycle
    endif

    ! Append the UTF-8 bytes to the utf8_bytes string
    do j = 1, num_bytes
      ! The utf8_bytes string is built byte by byte using char with kind=ascii.
      utf8_bytes(utf8_index:utf8_index) = char(utf8_code(j), kind=ascii)
      utf8_index = utf8_index + 1
    enddo
  enddo

  string=trim(utf8_bytes(1:utf8_index-1)) ! the UTF-8 byte sequence

end subroutine codepoints_to_utf8

function utf8_to_ucs4(string) result(corrected)
! return a string of kind ucs4 from bytes representing utf8 glyphs
character(len=*),intent(in)             :: string
character(len=:,kind=ucs4),allocatable  :: corrected
integer,allocatable                     :: codepoints(:)
integer                                 :: i, n
integer                                 :: err
   call utf8_to_codepoints(string,codepoints,err)
   n=size(codepoints)
   allocate(character(len=n,kind=ucs4)  :: corrected)

   if(.not.allocated(codepoints))then
      corrected=''
      return
   endif
   do i=1,n
      corrected(i:i)=char(codepoints(i),kind=ucs4)
   enddo
end function utf8_to_ucs4

function ucs4_to_utf8(ucs4_string,err) result(string)
! return bytes representing utf8 glyphs from a string of kind ucs4
character(len=*,kind=ucs4),intent(in)   :: ucs4_string  ! stores the UCS-4 string.
integer, intent(out),optional           :: err
character(len=:,kind=ascii),allocatable :: string
integer                                 :: codepoints(len(ucs4_string))
integer                                 :: i

   codepoints=[(ichar(ucs4_string(i:i)),i=1,len(ucs4_string))]

   call codepoints_to_utf8(codepoints,string,err)

end function ucs4_to_utf8

function utf8_to_ucs4_via_io(string) result(corrected)
character(len=*),intent(in)            :: string
character(len=:,kind=ucs4),allocatable :: corrected
character(len=(len(string)),kind=ucs4) :: line
character(len=255)                     :: iomsg
integer                                :: i
integer                                :: lun
integer                                :: iostat
   open(newunit=lun,encoding='UTF-8',status='scratch')
   do i=1,len(string)
      write(lun,'(A)',iostat=iostat,iomsg=iomsg,advance='no')string(i:i)
      if(iostat.ne.0)then
         ! not definite: after an error the position may be undefined
         write(lun,'(A)',iostat=iostat,iomsg=iomsg,advance='no')'?'
	 write(stderr,'(A)')trim(iomsg)
      endif
   enddo
   write(lun,'(A)',advance='yes')
   rewind(lun)
   read(lun,'(A)',iostat=iostat)line
   close(lun)
   corrected=trim(line)
end function utf8_to_ucs4_via_io

function ucs4_to_utf8_via_io(ucs4_string) result(corrected)
character(len=*,kind=ucs4),intent(in)          :: ucs4_string
character(len=:,kind=ascii),allocatable        :: corrected
character(len=(len(ucs4_string)*4),kind=ascii) :: line
integer                                        :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')ucs4_string
   rewind(lun)
   open(unit=lun,encoding='default')
   read(lun,'(A)')line
   close(lun)
   corrected=trim(line)
end function ucs4_to_utf8_via_io

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

end module M_utf8
