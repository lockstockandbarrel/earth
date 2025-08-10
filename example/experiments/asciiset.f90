program asciiset
use iso_fortran_env, only : stdout=>output_unit
use M_utf8
use odessa_unicode
implicit none

intrinsic selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')

character(len=26, kind=ascii )         :: alphabet
character(len=30, kind=ucs4  )         :: hello_world
character(len=30, kind=ucs4  )         :: string

character(len=:),allocatable           :: aline
character(len=:,kind=ucs4),allocatable :: uline
character(len=1),allocatable           :: ch(:)
character(len=1,kind=ucs4),allocatable :: glyph(:)
integer                                :: i
integer                                :: nerr

! Do the extended character set values change from ADE 128 to 255
! depending on the file encoding? By the standard, I think they
! should all become a character indicating the character is not
! valid(?). Check on screen and when redirected to file. Doing latin   
! or latin1 or latin2 conversion would make sense as well.

      write(stdout,'(a)') 'default encoding default kind:'
      write(stdout,'(10(g0,1x,g0,1x))')(char(i),i=0,255)

      write(stdout,'(a)') 'default encoding kind=ascii:'
      write(stdout,'(10(g0,1x,g0,1x))')(char(i,kind=ascii),i=0,255)

      write(stdout,'(a)') 'explicitly default encoding default kind:'
      open (stdout, encoding='DEFAULT')
      write(stdout,'(10(g0,1x,g0,1x))')(char(i),i=0,255)

      write(stdout,'(a)') 'default encoding kind ucs4:'
      open (stdout, encoding='UTF-8')
      write(stdout,'(10(g0,1x,g0,1x))')(char(i),i=0,255)
      write(stdout,'(10(g0,1x,g0,1x))')(char(i,kind=ucs4),i=0,255)
      
      write(stdout,'(a)') 'utf8_to_utf32:'
      ch= [(char(i),i=0,255)]
      call utf8_to_utf32(ch,glyph,nerr)
      write(stdout,'(10(g0,1x,g0,1x))')glyph

      write(stdout,'(a)') 'utf8_to_ucs4_via_io:'
      write(stdout,'(10(g0,1x,g0,1x))')(utf8_to_ucs4_via_io(char(i)),i=0,255)

      write(stdout,'(a)') 'utf8_to_ucs4:'
      write(stdout,'(10(g0,1x,g0,1x))')(utf8_to_ucs4(char(i)),i=0,255)

end program asciiset
