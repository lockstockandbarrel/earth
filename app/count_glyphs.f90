program count_glyphs
! @(#) read a utf-8 file and write it out with lines prefixed with glyph count of the line
use, intrinsic :: iso_fortran_env, only : output_unit, input_unit
implicit none
intrinsic selected_char_kind
intrinsic is_iostat_end
intrinsic len_trim
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter             :: g= '(*(g0))'
integer                                :: length
integer                                :: i
integer                                :: iostat
character(len=4096,kind=ucs4)          :: uline ! specifies maximum line length of 4096 bytes,
                                                ! which might be as few as 1024 (ie. 4096/4) glyphs
character(len=255)                     :: iomsg

   open (input_unit, encoding='UTF-8')
   open (output_unit, encoding='UTF-8')

   do
      read(input_unit,'(a)',iostat=iostat,iomsg=iomsg)uline
      if(iostat.eq.0)then
         length=len_trim(uline)
         write(output_unit,'(i9,": ",a)')length,uline(:length)
      elseif(is_iostat_end(iostat))then
         exit
      else 
         ! does the message have to be converted to ucs-4?
         write(output_unit,g)'<ERROR>',trim(iomsg)
         exit
      endif
   enddo

end program count_glyphs
