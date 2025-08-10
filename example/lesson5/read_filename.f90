program read_filename
! @(#) convert ucs-4 filename to utc-8 for OPEN() statement
use, intrinsic :: iso_fortran_env, only : output_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=:),allocatable           :: afilename
character(len=:,kind=ucs4),allocatable :: ufilename
integer                                :: lun

   ! we have a UCS-4 filename from somewhere ...
   ufilename = & ! ENCODING:môj_obľúbený_súbor "my_favorite_file"
   char(int(z'6D'),kind=ucs4)  // char(int(z'F4'),kind=ucs4) // char(int(z'6A'),kind=ucs4)// &
   char(int(z'5F'),kind=ucs4)  // char(int(z'6F'),kind=ucs4) // char(int(z'62'),kind=ucs4)// &
   char(int(z'13E'),kind=ucs4) // char(int(z'FA'),kind=ucs4) // char(int(z'62'),kind=ucs4)// &
   char(int(z'65'),kind=ucs4)  // char(int(z'6E'),kind=ucs4) // char(int(z'FD'),kind=ucs4)// &
   char(int(z'5F'),kind=ucs4)  // char(int(z'73'),kind=ucs4) // char(int(z'FA'),kind=ucs4)// &
   char(int(z'62'),kind=ucs4)  // char(int(z'6F'),kind=ucs4) // char(int(z'72'),kind=ucs4)

   afilename=ucs4_to_utf8(ufilename)

   open (newunit=lun, file=afilename, encoding='utf-8')
   !CLOSE(unit=lun, status='delete')

contains

function ucs4_to_utf8(ucs4_string) result(ascii_string)
character(len=*,kind=ucs4),intent(in) :: ucs4_string
character(len=:),allocatable          :: ascii_string
character(len=(len(ucs4_string)*4))   :: line
integer                               :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')ucs4_string
   rewind(lun)
   open(unit=lun,encoding='default')
   read(lun,'(A)')line
   close(lun)
   ascii_string=trim(line)
end function ucs4_to_utf8

end program read_filename
