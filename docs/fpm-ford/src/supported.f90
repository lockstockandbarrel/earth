program supported
! @(#) report if ISO_10646 is supported
use iso_fortran_env, only : output_unit, character_kinds
implicit none

intrinsic selected_char_kind

integer :: i
integer :: iostat

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')
   write(*,*)'Supported character kinds', character_kinds

   write(*,*)'ASCII     ',merge('Supported    ','Not Supported',ascii /= -1),  'KIND=',ascii
   write(*,*)'ISO_10646 ',merge('Supported    ','Not Supported',ucs4 /= -1),   'KIND=',ucs4
   write(*,*)'DEFAULT   ',merge('Supported    ','Not Supported',default /= -1),'KIND=',default

   if(default.eq.ascii)then
       write(*,*)'ASCII is the same as the default on this processor'
   endif

   open(output_unit,encoding='utf-8',iostat=iostat)
   if(iostat.ne.0)then
      write(*,*)'utf-8 encoding not supported'
   else
      write(*,*)'7-bit ASCII'
      write(*,'(16(i3,1x,a,1x))')(i,char(i,kind=ucs4),i=0,127)
      write(*,*)'remaining 8-bit ASCII'
      write(*,'(16(i3,1x,a,1x))')(i,char(i,kind=ucs4),i=128,255)
   endif

end program supported
