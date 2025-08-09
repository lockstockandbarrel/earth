program euro
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none
integer, parameter         :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter :: g5='(*(t3,g0,t9,g0,t16,g0,t22,g0))'
character(len=1)           :: euro0 = '€'
character(len=*),parameter :: euro00 = '€'
character(len=1,kind=ucs4) :: euro1 = '€'
character(len=1,kind=ucs4) :: euro2 = ucs4_'€'
character(len=1,kind=ucs4) :: euro3 = char(int(z'20AC'), kind=ucs4)
      write(stdout,g5) 'OUTPUT  LEN    BYTES    KIND'
      open(stdout,encoding='utf-8')
      write(stdout,g5)euro0,len(euro0),storage_size(euro0)/8,kind(euro0)
      write(stdout,g5)euro00,len(euro00),storage_size(euro00)/8,kind(euro00)
      write(stdout,g5)euro1,len(euro1),storage_size(euro1)/8,kind(euro1)
      write(stdout,g5)euro2,len(euro2),storage_size(euro2)/8,kind(euro2)
      write(stdout,g5)euro3,len(euro3),storage_size(euro3)/8,kind(euro3)
end program euro
