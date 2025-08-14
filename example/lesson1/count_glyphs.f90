program count_glyphs
! @(#) read a utf-8 file and write it out with lines prefixed with glyph counts of the line
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none
intrinsic selected_char_kind
intrinsic is_iostat_end
intrinsic len_trim
!------
! NOTE: we will be using the kind name "ucs4" for Unicode variables
integer, parameter            :: ucs4 = selected_char_kind ('ISO_10646')
!------
character(len=*),parameter    :: g= '(*(g0))'
integer                       :: length
integer                       :: i
integer                       :: iostat
!------
! NOTE: this character variable is the Unicode kind, not ASCII
character(len=1024,kind=ucs4) :: uline ! specifies maximum line length of 1024 glyphs
                                       ! which would be 4*1024 bytes
!------
character(len=255)            :: iomsg

   !------
   ! NOTE: you can change the encoding used for a file dynamically, even on pre-assigned files
   open (stdin, encoding='UTF-8')
   open (stdout, encoding='UTF-8')
   !------

   do
      read(stdin,'(a)',iostat=iostat,iomsg=iomsg)uline
      if(iostat.eq.0)then
         !------
         ! NOTE: LEN_TRIM() works with UCS-4 just as with ASCII
         length=len_trim(uline)
         !------
         !------
         ! NOTE: String substrings work just like with ASCII as well
         write(stdout,'(i9,": ",a)')length,uline(:length)
         !------
      elseif(is_iostat_end(iostat))then
         exit
      else
         !------
         ! NOTE:
         ! does the ASCII message have to be converted to UCS-4?
         ! This will be discussed in detail later, but for now
         ! remember you can change the encoding of a file dynamically
         open (stdout, encoding='DEFAULT')
         !------
         write(stdout,g)'<ERROR>',trim(iomsg)
         exit
      endif
   enddo

end program count_glyphs
