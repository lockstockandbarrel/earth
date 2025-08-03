program uni_to_ftn
! simplistically copy a utf8 formatted sequential file from stdin to stdout, prefixing lines with number of glyphs
use, intrinsic :: iso_fortran_env, only : output_unit,input_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=1024,kind=ucs4)          :: uline ! arbitrarily assume maximum line length. Stream I/O could be used instead
character(len=:,kind=ucs4),allocatable :: ustr
integer                                :: iostat
   open (output_unit, encoding='UTF-8')
   open (input_unit, encoding='UTF-8')
   do
      read(input_unit,'(a)',iostat=iostat)uline
      if(iostat.ne.0)exit
      ustr=trim(uline)                                 ! assign and trim() works on 4-byte characters
      write(output_unit,'(i9.9,":",a)')len(ustr),ustr  ! len()  works on 4-byte characters
   enddo
end program uni_to_ftn
