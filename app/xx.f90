program count_glyphs
implicit none
integer             :: length, iostat
character(len=4096) :: aline
character(len=255)  :: iomsg
   do
      read(*,'(a)',iostat=iostat,iomsg=iomsg)aline
      if(iostat.eq.0)then
         length=len_trim(aline)
         write(*,'(i9,": ",a)')length,aline(:length)
      elseif(is_iostat_end(iostat))then
         exit
      else 
         write(*,'(a)')'<ERROR>',trim(iomsg)
         exit
      endif
   enddo
end program count_glyphs
