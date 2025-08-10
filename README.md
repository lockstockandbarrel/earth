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

 + utf8_to_codepoints
 + utf8_to_ucs4
 + utf8_to_ucs4_via_io
 + ucs4_to_utf8

Fortran supports just using 4-byte UCS-4 encoding for characters/glyphs quite
well, including an option to automatically encode and decode data read and written
to UTF-8 files. Where the issues start is that the standard specifies default 
character kinds for the OPEN FILE= specifier, filenames on INQUIRE, strings returned
by GET_COMMAND_ARGUMENT, ... . So you can not just forget about ASCII quite yet.
And some systems might use other encodings like UTF-16, so basically Fortran still
needs procedures or methods for converting between UCS-4, UTF-16, UTF-8, and possibly
other encodings before you can just use Unicode everywhere in Fortran and forget about
ASCII or other encodings. Plus there apparently is currently a dearth of documentation 
about using Unicode from Fortran. This material is meant to fill in the gap with both
issues.

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
## Fortran Unicode Tutorial

Here are some lessons that describe my experiences with Unicode and
Fortran, including discussions of open questions concerning what is
standardized, what extensions commonly used compilers provide to address
some of the current gaps, and what is known to be non-portable but useful
behavior from some compilers regarding undefined behaviors.

**WIP Notice:** this has barely been begin. I am still sorting out what is and is
not standard.

Once constructed and stable, the lessons will become a tutorial on the Fortran Wiki,
and this project will contain the related modules and examples, but at this point
this is just an incomplete outline so starting the Wiki entries would be premature.

### Introduction to Fortran Unicode support
   + [**Lesson I:**](docs/lesson1_ucs4.md) reading and writing UTF-8 Unicode files
   + [**Lesson II:**](docs/lesson2_ucs4.md) creating Unicode strings in ASCII Fortran source files
   + Lesson III: mixing ASCII and UCS4 kinds as regards assignments,
                 concatenation, passing arguments to external ASCII libraries, and I/O argument lists
   + [**Lesson IV:**](docs/lesson4_ucs4.md)what is and is not supported with internal READ and WRITE statements
   + [**Lesson V:**](docs/lesson5_ucs.md)processing Unicode file names on OPEN() statements
   + [**Lesson VI:**](docs/lesson6_ucs4.md) reading UTF-8 strings from command lines
   + Lesson VII:  passing Unicode strings to and from C
   + Lesson VIII: related utility programs

### off the beaten path:
   + Lesson I: UTF-8 source files -- just in comments and constants
   + Lesson II: the backslash escape code extension
   + Lesson III: converting between UCS-4 and UTF-8 with procedures
   + Lesson IV: embedding BOM characters at the beginning of files

### Processing Unicode when ISO-10646 is not supported by a compiler
   + Lesson I: converting UTF-8 codes to and from INTEGER values
   + Lesson II: byte-oriented printing of 4-byte integers
   + Lesson III: issues with terminal emulators, system locale settings, and
                  other Unicode-related issues
   + Lesson IV: working with ASCII extended encodings; particularly those
                 commonly referred to as Extended, Latin, Latin1 and Latin2.
-------------------------------------------------------------

## Dusty Corners
Assigns between different character kinds, and an apparent
lack of an ability to specify encoding='UTF-8' when using
internal reads and writes like you can with an OPEN(3f)
statement are still a little murky; as well as how the T
field descriptor works with multi-byte characters.

Note that at least with the gfortran(1) compiler conversion
to UCS4 internal representation works automatically when
reading and writing from files with encoding='utf-8' specified,
this is just primarily concerned with UTF-8 string constants occurring
in the code files themselves.

## backslash extension

The gfortran(1) compiler supports an extension that allows for building
ucs4 strings more easily than using BOZ literals.

The following example prints the Unicode symbol ☻ (black smiling face)
of code point U+263B. The compiled binary must be executed in a terminal
with Unicode support, like XTerm or sakura.

```fortran
program main ! code to place in unicode.f90
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    integer, parameter :: ucs4 = selected_char_kind('ISO_10646')
    character(kind=ucs4, len=:), allocatable :: str

    ! GFORTRAN EXTENSION:
    str = ucs4_'Unicode character: \u263B'

    open (output_unit, encoding='utf-8')
    print '(a)', str
end program main
```

Build and run the executable with:

```bash
$ gfortran -fbackslash -o unicode unicode.f90
$ ./unicode
Unicode character: ☻
```

The -fbackslash compiler flag is required for escaped Unicode
characters. Otherwise, the type conversion has to be done manually using
BOZ literals, for instance:
```text
str = ucs4_'Unicode character: ' // char(int(z'263B'), kind=ucs4)
```

Or, simply by using the decimal value of the character code point, without BOZ literal:

```text
str = ucs4_'Unicode character: ' // char(9787, kind=ucs2)
```
## See Also

 + [https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949](https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949)
 + [https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618](https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618)
 + [https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764](https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764)

 Other languages, such as the Python encode() and decode() procedures, supply related functionality.

```bash
# extract discourse as text
lynx --dump https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949
lynx --dump https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764
```
