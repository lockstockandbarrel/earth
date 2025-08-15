program demo_selected_char_kind
! @(#) report if ISO_10646 is supported
use iso_fortran_env
implicit none

intrinsic selected_char_kind

integer :: i
integer :: iostat

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')

   write(*,*)
   write(*,*)'print 1-byte values (from 0 to 255) as characters:'
   write(*,*)' only 0 to 127 is defined in ASCII, 128-255'
   write(*,*)' often used for extended ASCII such as latin1 or latin2 encoding, or extended'
   write(*,*)' characters such as box characters. If your default output is utf-8 values above'
   write(*,*)' 127 should be a question mark or a shaded box'

!  ENCODING= might not be supported
   open(output_unit,encoding='utf-8',iostat=iostat)
   if(iostat.ne.0)then
      write(*,*)'utf-8 encoding not supported'
   else
      write(*,'(10(i3,1x,a,1x))')(i,char(i,kind=ucs4),i=0,255)
   endif

   write(*,*)
   write(*,*)'printing the bytes directly'

   write(*,*)'with encoding="ASCII"'
   open(output_unit,encoding='ascii',iostat=iostat)
   write(*,*)'ADE:'
   write(*,'(10(i3,1x,a,1x))')(i,i,i=0,255)
   write(*,*)'CHAR():'
   write(*,'(10(i3,1x,a,1x))')(i,char(i),i=0,255)
   write(*,*)'ACHAR():'
   write(*,'(10(i3,1x,a,1x))')(i,achar(i),i=0,255)

   write(*,*)'UCS-4 with encoding="UTF-8"'
   open(output_unit,encoding='utf-8',iostat=iostat)
   write(*,'(10(i3,1x,a,1x))')(i,char(i,kind=ucs4),i=0,255)

   write(*,*)'with encoding="UTF-8"'
   open(output_unit,encoding='utf-8',iostat=iostat)
   write(*,'(10(i3,1x,a,1x))')(i,i,i=0,255)

   write(*,*)'Smiling face with open mouth',char(int(z'1F603'),kind=ucs4) ! 😃
   write(*,*)'Smiling face with open mouth',char([int(z'F0'),int(z'9F'),int(z'98'),int(z'83')],kind=ascii) ! 😃

end program demo_selected_char_kind
