program test_utf8

use iso_fortran_env, only : output_unit
use M_utf8, only : utf8_to_ucs4, utf8_to_codepoints, utf8_to_ucs4_via_io
implicit none
character(len=100)                     :: s
integer, allocatable                   :: codepoints(:)
integer                                :: i, err
intrinsic selected_char_kind
integer, parameter                     :: ucs4  =   selected_char_kind ('ISO_10646')
character(len=*),parameter             :: not_ascii = "ľščťžýáßĄĘ®™√🙂"
character(len=:,kind=ucs4),allocatable :: corrected
character(len=1,kind=ucs4)             :: letter   

   s = "Héllo 🌍"  ! UTF-8 input string

   call utf8_to_codepoints(s, codepoints, err)

   if (err /= 0) then
     print *, "Decode error:", err
   else
     print *, "Code points:"
     do i = 1, size(codepoints)
       write(*,'("U+",Z8.8)') codepoints(i)
     enddo
   endif
   !Code points:
   !U+00000048
   !U+000000E9
   !U+0000006C
   !U+0000006C
   !U+0000006F
   !U+0001F30D

   open (output_unit, encoding='UTF-8')

   write(*,*)
   call utf8_to_codepoints(not_ascii,codepoints,err)
   write(*,*)'SIZE:',size(codepoints), merge('PASSED','FAILED',size(codepoints)==14)
   write(*,'(*(a))')char(codepoints,kind=ucs4)
   write(*,'("UNICODE NOTATION:",T20,*("U+",z6.6:," "))')codepoints
   write(*,'("HTML:",T20,*("&#x",z0,";":))')codepoints
   write(*,'("C/PYTHON:",T20,*("\U",z8.8:))')codepoints

   write(*,*)
   write(*,*)'properly converted to ucs4'
   corrected=utf8_to_ucs4(not_ascii)
    write(*,*)'CORRECTED:',corrected
    write(*,*)'LEN CORRECTED:',len(corrected)
    write(*,*)'STORAGE_SIZE:',storage_size(corrected)
    write(*,*)'so now slicing, intrinsics, ... work!!'
    letter=corrected(14:14)
    write(*,*)'14th character',letter
    write(*,*)'index position',index(corrected,letter)

   write(*,*)
   write(*,*)'properly converted to ucs4 via I/O'
   corrected=utf8_to_ucs4_via_io(not_ascii)
    write(*,*)'CORRECTED:',corrected
    write(*,*)'LEN CORRECTED:',len(corrected)
    write(*,*)'STORAGE_SIZE:',storage_size(corrected)
    write(*,*)'so now slicing, intrinsics, ... work!!'
    letter=corrected(14:14)
    write(*,*)'14th character',letter
    write(*,*)'index position',index(corrected,letter)

   write(*,*)
   write(*,*)'variable= &'
   write(*,'(("  char(int(z''",z0,"''),kind=ucs4)",:,", &"))')(corrected(i:i),i=1,len(corrected))

end program
