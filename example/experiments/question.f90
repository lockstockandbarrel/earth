program shouldit
use iso_fortran_env, only : output_unit, int64
implicit none
intrinsic selected_char_kind
integer, parameter :: ucs4   =   selected_char_kind ('ISO_10646')
integer, parameter :: ascii  =   selected_char_kind ('ASCII')

! can something in quotes only be encoded as ASCII?
!NO!character(len=*,kind=ucs4),parameter :: A=ucs4_'ľščťžýáßĄĘ®™√🙂'
!NO!character(len=*,kind=ucs4),parameter :: A='ľščťžýáßĄĘ®™√🙂'
character(len=*,kind=ascii),parameter :: A='ľščťžýáßĄĘ®™√🙂'
character(len=512,kind=ucs4) :: ULINE
character(len=512,kind=ascii) :: ALINE

character(len=*,kind=ucs4),parameter :: B= &
   char(int(z'13E'),kind=ucs4)//   &
   char(int(z'161'),kind=ucs4)//   &
   char(int(z'10D'),kind=ucs4)//   &
   char(int(z'165'),kind=ucs4)//   &
   char(int(z'17E'),kind=ucs4)//   &
   char(int(z'FD'),kind=ucs4)//    &
   char(int(z'E1'),kind=ucs4)//    &
   char(int(z'DF'),kind=ucs4)//    &
   char(int(z'104'),kind=ucs4)//   &
   char(int(z'118'),kind=ucs4)//   &
   char(int(z'AE'),kind=ucs4)//    &
   char(int(z'2122'),kind=ucs4)//  &
   char(int(z'221A'),kind=ucs4)//  &
   char(int(z'1F642'),kind=ucs4)

!character(len=1,kind=ucs4),parameter :: C(*)=&
!char(int([z'13E',z'161',z'10D',z'165',z'17E',z'FD',z'E1',z'DF',z'104',z'118',z'AE',z'2122',z'221A',z'1F642']),kind=ucs4)

   open (output_unit, encoding='UTF-8')

   write(*,*)'this is not really UTF-8, it a stream of bytes that has the right values'
   write(*,*)A,len(A),storage_size(A),A(10:14)

   write(*,*)'Fortran actually understands this, so slice and intrinsics work correctly'
   write(*,*)B,len(B),storage_size(B),B(10:14)

   write(*,*)'The data between the quotes is ASCII so this does not work'
   write(*,*)ucs4_'ľ', ucs4_'š', ucs4_'č', ucs4_'ť', ucs4_'ž', &
             ucs4_'ý', ucs4_'á', ucs4_'ß', ucs4_'Ą', ucs4_'Ę', &
             ucs4_'®', ucs4_'™', ucs4_'√', ucs4_'🙂'

   write(*,*)'might work but not defined by the standard that I can tell'
   write(ULINE,'(A)') A
   write(*,*) 'ULINE:',trim(ULINE)
   write(*,*) 'A:',A

   write(*,*)'might work but not defined by the standard that I can tell'
   write(ALINE,'(A)') A
   write(*,*) 'ALINE:',trim(ALINE)

   write(*,*)'biggest code:'
   write(*,*) 'FFFFFFFF:',int(z'FFFFFFFF',kind=int64)

end program shouldit
