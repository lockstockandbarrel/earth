# M_utf8

## A WIP (Work In Progress)

Fortran does not inherently provide a direct built-in function to
convert an arbitrary UTF-8 encoded string to UCS-4 (ISO_10646) character
kind. While Fortran 2003 and later standards introduce support for
different character kinds, including ISO_10646 (UCS-4), the conversion
from a byte-stream representing UTF-8 to the UCS-4 character kind typically
requires manual decoding accept when reading and writing from a file with
encoding set to "UTF-8".

Herein are some experiments in helping to facilitate using source codes
containing UTF-8 encoded strings as standard Fortran Unicode characters
instead of as ASCII byte streams that contain UTF-8 constant strings, such
as the procedures

### M_utf8 module

 + utf8_to_codepoints, codepoints_to_utf8

 + utf8_to_ucs4, ucs4_to_utf8
 + utf8_to_ucs4_via_io, ucs4_to_utf8_via_io

 + ascii_to_ucs4, ucs4_to_ascii

 + isolatin_to_unicode, unicode_to_isolatin
 + utf8_to_isolatin, isolatin_to_utf8
 + isolatin_to_utf32, utf32_to_isolatin

Initially based on a discussion begun in
https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949, 2025-08;
including features and enhancements from Francois Jacq.

The improvements include procedures for handling ASCII encoding extensions
often used for internationalization that pre-date Unicode, such as the
Latin encoudings.

## Purpose

Fortran optionally supports internal representation of Unicode using
4-byte-per-character UCS-4 encoding for characters/glyphs quite well,
including an option to automatically encode and decode data read and
written to UTF-8 files. 

Where the issues start is that the standard
specifies default character kinds for the OPEN FILE= specifier, filenames
on INQUIRE, strings returned by GET_COMMAND_ARGUMENT, ... . So you can
not just forget about ASCII quite yet.  And some systems might use other
encodings like UTF-16, so basically Fortran still needs procedures or
methods for converting between UCS-4, UTF-16, UTF-8, and possibly other
encodings before you can just use Unicode everywhere in Fortran and forget
about ASCII or other encodings.

The intent is for the information to useful for all Fortran processing, but currently
is primarily tested with gfortran on Linux and Cygwin.

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
-------------------------------------------------------------
## Tutorial

There apparently is currently a dearth of documentation about using
Unicode from Fortran, so as this project progresses a companion tutorial
is being produced as well that first describes the general challenges of
using Unicode from Fortran when supported; and then using it from utf-8 files
using Unicode when not supported.

   [Unicode Tutorial](docs/lesson0.md)
-------------------------------------------------------------
## See Also

 + [https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949](https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949)
 + [https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618](https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618)
 + [https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764](https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764)
<!--
 + [UTF-8 Everywhere Manifesto](http://utf8everywhere.org/)
-->

 Other languages, such as the Python encode() and decode() procedures, supply related functionality.
