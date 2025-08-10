## Introduction to Fortran Unicode support
### Lesson V: processing Unicode file names on OPEN() statements

If your OS supports utf-8 as the default encoding it is likely you will
at some point encounter a filename containing multi-byte Unicode characters.

The definition of the OPEN() statement specifies that the
filename expression is a "scalar-default-char-expr". But it
also states

      A file may have a name; a file that has a name is called a named
      file. The name of a named file is represented by a character
      string value. The set of allowable names for a file is processor
      dependent.

And the description of the FILE= specifier states

      12.5.6.10   FILE= specifier in the OPEN statement

      The value of the FILE= specifier is the name of the file to
      be connected to the specified unit. Any trailing blanks are
      ignored. The file-name-expr shall be a name that is allowed by
      the processor. The interpretation of case is processor dependent.

So what filenames are allowed is processor-dependent -- but probably
is restricted to a default character expression, which currently is
typically ASCII or extended ASCII.

So if your Fortran compiler allows Unicode filenames the filename is
likely to require being specified as a stream of bytes of the default
CHARACTER kind representing utf-8 characters.

But what if you have the filename in UCS-4 internal representation?
Fortran does not currently provide an intrinsic procedure for converting
ucs-4 to utf-8 Unicode.

Fortran does the conversion needed when writing ucs-4 internal data
to utf-8-encoded files. We can use that functionality to create a
simple conversion routine.

```fortran
program read_filename
! @(#) convert ucs-4 filename to utc-8 for OPEN() statement
use, intrinsic :: iso_fortran_env, only : output_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=:),allocatable           :: afilename
character(len=:,kind=ucs4),allocatable :: ufilename
integer                                :: lun

   ! we have a UCS-4 filename from somewhere ...
   ufilename = & ! ENCODING:môj_obľúbený_súbor "my_favorite_file"
   char(int(z'6D'),kind=ucs4)  // char(int(z'F4'),kind=ucs4) // char(int(z'6A'),kind=ucs4)// &
   char(int(z'5F'),kind=ucs4)  // char(int(z'6F'),kind=ucs4) // char(int(z'62'),kind=ucs4)// &
   char(int(z'13E'),kind=ucs4) // char(int(z'FA'),kind=ucs4) // char(int(z'62'),kind=ucs4)// &
   char(int(z'65'),kind=ucs4)  // char(int(z'6E'),kind=ucs4) // char(int(z'FD'),kind=ucs4)// &
   char(int(z'5F'),kind=ucs4)  // char(int(z'73'),kind=ucs4) // char(int(z'FA'),kind=ucs4)// &
   char(int(z'62'),kind=ucs4)  // char(int(z'6F'),kind=ucs4) // char(int(z'72'),kind=ucs4)

   afilename=ucs4_to_utf8(ufilename)

   open (newunit=lun, file=afilename, encoding='utf-8')
   !CLOSE(unit=lun, status='delete')

contains

function ucs4_to_utf8(ucs4_string) result(ascii_string)
character(len=*,kind=ucs4),intent(in) :: ucs4_string
character(len=:),allocatable          :: ascii_string
character(len=(len(ucs4_string)*4))   :: line
integer                               :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')ucs4_string
   rewind(lun)
   open(unit=lun,encoding='default')
   read(lun,'(A)')line
   close(lun)
   ascii_string=trim(line)
end function ucs4_to_utf8

end program read_filename
```
## Summary

If your processor supports Unicode filenames you probably need to
convert any filename in UCS-4 encoding to UTF-8 encoding.

Using Fortran's ability to encode UCS-4 data as UTF-8 when writing
external files it is easy to create a function for converting between
the two encodings.

Note that modules of related functions can be found at
[github.com/lockstockandbarrel/earth](github.com/lockstockandbarrel/earth)

If your processor does not support Unicode filenames your operating system
may support links. So an alternative might be to make an ASCII filename
that is an alias for the unusable filename. This can typically be done
with system calls to the intrinsic EXECUTE_COMMAND_LINE() if you do not have
procedures for creating (and removing) links.

+ [NEXT](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson6_ucs4.md)
+ [PREVIOUS](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson4_ucs4.md)
