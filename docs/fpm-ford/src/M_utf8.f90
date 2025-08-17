module M_utf8
use iso_fortran_env, only: error_unit, stderr=>error_unit
use M_unicode, only : utf8_to_codepoints,  codepoints_to_utf8
! Unicode-related procedures requiring Fortran support of ISO-10646.
implicit none

private
public :: utf8_to_ucs4,        ucs4_to_utf8
public :: utf8_to_ucs4_via_io, ucs4_to_utf8_via_io
public :: ascii_to_ucs4,       ucs4_to_ascii
! imported from M_unicode
public :: utf8_to_codepoints,  codepoints_to_utf8

integer,parameter :: ucs4=selected_char_kind('ISO_10646') ! The compiler must support UCS-4 characters
integer,parameter :: ascii=selected_char_kind('ascii') ! maybe should use default, as ASCII is technically 128, not 256 chars

contains

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
character(len=:),allocatable            :: string
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
