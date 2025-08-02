# M_utf8

## A WIP (Work In Progress)

Fortran does not inherently provide a direct built-in function to
convert an arbitrary UTF-8 encoded string to UCS-4 (ISO_10646) character
kind. While Fortran 2003 and later standards introduce support for
different character kinds, including ISO_10646 (UCS-4), the conversion
from a byte-stream representing UTF-8 to the UCS-4 character kind typically
requires manual decoding accept when reading and writing from a file with
encoding set to "UTF-8".

Here are some experiments in helping to facilitate using source codes
containing UTF-8 encoded strings as standard Fortran Unicode characters
instead of as ASCII byte streams that contain UTF-8 constant strings.

 + utf8_to_codepoints
 + utf8_to_ucs4
 + utf8_to_ucs4_via_io
 + ucs4_to_utf8

```fortran
program testit
use iso_fortran_env, only : stdout=>output_unit
use M_utf8, only : utf8_to_ucs4
implicit none
integer,parameter                       :: ucs4 = selected_char_kind("ISO_10646")
character(len=:,kind=ucs4),allocatable  :: str

   open (stdout, encoding='UTF-8')
   
   ! standard method
   str  = ucs4_'Hello World and Ni Hao -- ' &
      // char (int (z'4F60'), ucs4)         &
      // char (int (z'597D'), ucs4)
   write (stdout,*) str

   ! converting pseudo-utf8 to ucs4
   str= utf8_to_ucs4('Hello World and Ni Hao -- 你好')
   write (stdout,*) str
   write (stdout,*) len(str)
   write (stdout,*) str(27:27)

end program testit
```
Assigns between different character kinds, and an apparent
lack of an ability to specify encoding='UTF-8' when using
internal reads and writes like you can with an OPEN(3f) 
statement are still a little murky; as well as how the T
field descriptor works with multi-byte characters. 

Note that at least with the gfortran(1) compiler conversion
to UCS4 internal representation works automatically when 
reading and writing from files with encoding='utf-8' specified,
this is just primarily concerned with UTF-8 string constants occuring
in the code files themselves.

## See Also

 + [https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949](https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949)
 + [https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618](https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618)
 + [https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764](https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764)

 Other languages, such as the Python encode() and decode() procedures, supply related functionality.
