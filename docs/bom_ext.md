## Byte Order Mark (BOM):

## Background

At its simplest, Unicode basically assigns a unique integer code to
each glyph or character. Basically ASCII files are just that -- composed
of one-byte unsigned integer values. But this limits you to 255 characters.

So the ISO/IEC-10646 specification defines several ways of encoding
Unicode characters as computer bytes (ie., UTF-8, UTF-2/UTF-16, and
UCS-4/UTF-32).  Each method has pros and cons, primarily being trade-offs
between the amount of data required versus how simply or efficiently the
data can be processed; but including other factors such as how well it
integrates and how little it conflicts with existing common text encoding
such as ASCII.

So to make it simple to identify which encoding a text file is using,  a 
byte sequence was designed called the Byte Order Mark (BOM) that would
not act as a character itself, but could be used to determine what 
encoding the file was using when placed at the start of the file.

That is why some Unicode files, particularly those originating from Windows
systems, begin with a Byte Order Mark (BOM). This Unicode "noncharacter"
is U+FEFF. When encoded, it provides a strong indicator of the encoding
and byte order (endianness).

   + UTF-8: The encoded BOM is bytes EF BB BF.
   + UTF-16 Big Endian: The encoded BOM is bytes FE FF.
   + UTF-16 Little Endian: The encoded BOM is bytes FF FE.
   + UTF-32 Big Endian: The encoded BOM is bytes 00 00 FE FF.
   + UTF-32 Little Endian: The encoded BOM is bytes FF FE 00 00.

If a BOM is absent, one can attempt to decode the file using each encoding
and check for validity. For instance, a sequence of bytes that is valid
UTF-8 might be invalid or produce nonsensical characters when interpreted
as UTF-16 or UTF-32. Because of Unicodes' design this can be done with a
high degree of reliability, decreasing the need for the "Unicode Signature"
defined by the magic string of bytes placed at the beginning of the file.

In practice UTF-8 has many advantages over the other encodings when
used for file data. It is not effected by endianness, contains
ASCII 8-bit characters as a subset, can avoid being misconstrued as
an extended ASCII character set such as LATIN1 or LATIN2 encoding
(commonly used with modern European languages), and can represent all
Unicode characters but remain as compact as ASCII files when the files
predominantly are composed of ASCII characters, which is still often
the case.

Note __all the Unicode encodings are sensitive to byte order 
except UTF-8__. That alone might make UTF-8 the preferred text file format.

So UTF-8 has become so dominant as the Unicode file encoding scheme the
use of a BOM character is no longer even recommended unless required
to work properly with particular applications. Even
when a file is started with encoding='UTF-8', a Byte Order Mark (BOM)
is not generated automatically by any current Fortran compiler.

Note BOM characters are found most often in MicroSoft Windows environments
than others.  The BOM character as a magic number was used in virtually
all Unicode files initially, partly because Microsoft supported multiple
Unicode text file formats early on, before UTF-8 was seen as the de-facto
text file encoding.

If the BOM appears in the middle of a
data stream. Unicode says it should be interpreted as a normal
codepoint (namely a word joiner), not as a BOM.

### BOM as it applies to Fortran source files

The Unicode standard doesn't require a BOM for UTF-8, but it may be
encountered at the start of files. 

Its presence interferes with the use of UTF-8
by software that does not expect non-ASCII bytes at the start of a
file but that could otherwise handle the text stream.

The Unicode Standard permits the BOM in UTF-8, but does not
require or recommend its use.

This can include Fortran source where multi-byte characters are non-standard
as part of the coding instructions, but are often handled when appearing in
comments and literal quoted character strings.

Some applications may require it. The most relevant issue that the NAG
compiler has an extension where it formally supports UTF-8 source files,
which are supposed to require with a BOM character to distinguish them
from ASCII files.

## BOM as it regards Fortran source files

Determining whether a text is encoded in UTF-8, UTF-16, or UTF-32,
especially without explicit metadata, often relies on analyzing the byte
sequence for patterns specific to each encoding.

The standard also does
not recommend removing a BOM when it is there, so that round-tripping
between encodings does not lose information, and so that code that
relies on it continues to work. 

Not using a BOM allows text to be backwards-compatible with software
designed for extended ASCII. For instance many programming
languages permit non-ASCII bytes in string literals but not
at the start of the file.

```fortran
program bom_exe
!
! create a Fortran source file starting with a utf-8 BOM to see if your
! compiler will compile it or fail because a character is not in the
! Fortran character set outside of a comment or literal string
!
use iso_fortran_env, only : stdout => output_unit
implicit none
character(len=*),parameter :: &
   & A_bom = char(int(z'EF'))// char(int(z'BB'))// char(int(z'BF'))

   write(stdout,'(a)',advance='no')A_bom
   write(stdout,'(a)') &
    'program testit ! Unicode BOM as utf-8 bytes'               ,&
    '   write(*,*)"File starts with BOM from ""bytes"" write!"' ,&
    'end program testit'

end program bom_exe
```

```fortran
program bom_exe
!
! create a Fortran source file starting with a utf-8 BOM to see if your
! compiler will compile it or fail because a character is not in the
! Fortran character set outside of a comment or literal string
!
use iso_fortran_env, only : stdout => output_unit
implicit none
intrinsic selected_char_kind
integer,parameter :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*,kind=ucs4),parameter :: U_bom=char(int(z'FEFF'),kind=ucs4)

   open(stdout,encoding='UTF-8')
   write(stdout,'(a)',advance='no')U_bom
   write(stdout,'(a)') &

    ucs4_'program testit ! Unicode BOM encoded to utf-8 bytes by Fortran' ,&
    ucs4_'   write(*,*)"File starts with BOM from UCS-4 write!"'          ,&
    ucs4_'end program testit'

end program bom_exe
```
### References

See [Wikipedia](https://en.wikipedia.org/wiki/Byte_order_mark) for
more information on the Unicode character code, U+FEFF 
(aka. ZERO WIDTH NO-BREAK SPACE), 
