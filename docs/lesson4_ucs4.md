## Introduction to Fortran Unicode support
### Lesson IV:  what is and is not supported with internal READ and WRITE statements

   From the 2023 Fortran standard:

      Fortran 90 permitted deﬁned assignment between character strings
      of the same rank and different kinds.
   
      This document does not permit that if both of the different kinds are
      ASCII, ISO 10646, or default kind.  An input/output list shall
      not contain an effective item that is nondefault character except
      for ISO 10646 or ASCII character if the data transfer statement
      specifies an internal file of ISO 10646 character kind.
   
      An input/output list shall not contain an effective item of type
      character of any kind other than ASCII if the data transfer statement
      speciﬁes an ASCII character internal file.
   
      An output list shall not contain an effective item that is a
      boz-literal-constant.

   Would expect you could do encoding to and from utf8 and ucs4 as you
   can with an external file. Internal files are the standard method for
   converting numeric values to and from string variables, so that would
   seem natural.

   Would expect the A format would produce a binary copy of bytes when
   the kind is ASCII as it does with other types whether the internal file
   is UCS4 or ASCII. The A format is the original TRANSFER() function with
   field descriptor control; allowing binary data to be placed in formatted
   files, for example. Yes, but only for variables of the same kind. Using
   other CHARACTER kinds that do not match the record are not allowed. 

   There is no conversions between CHARACTER kinds that can be done with
   internal files!

   The second paragraph quoted from the standard above makes it seem that
   perhaps you can write ASCII bytes into a ISO 10646 variable, but it is
   unclear whether that would be a byte-per-byte transfer or whether only
   the 128 ASCII characters would be allowed or whether it might treat the
   data as UTF-8 and encode it into UCS4. In all compilers I have tried 
   the ASCII writes into a UCS4 variable fail to do any of those that I can
   determine.
```fortran
program internal_io
use iso_fortran_env, only : stdout=>output_unit
implicit none
integer,parameter                       :: ascii = selected_char_kind ("ascii")
integer,parameter                       :: ucs4 = selected_char_kind("ISO_10646")
character(len=1,kind=ucs4)              :: glyph
character(len=*),parameter              :: all='(*(g0))'
character(len=:,kind=ascii),allocatable :: aline
character(len=:,kind=ascii),allocatable :: astr
character(len=:,kind=ucs4),allocatable  :: uline
character(len=:,kind=ucs4),allocatable  :: ustr
integer                                 :: i

   open (stdout, encoding='UTF-8')
   
   print all, 'unicode UCS-4 string'
   ustr  = ucs4_'Hello World and Ni Hao -- ' // char(int(z'4F60'),ucs4) // char(int(z'597D'),ucs4)
   write (*,*) ustr
   print all, 'length  :',len(ustr)
   print all, 'bytes   :',storage_size(ustr)/8

   print all, 'ASCII bytes'
   astr= 'Hello World and Ni Hao -- 你好'
   write (*,*) astr
   print all, 'length  :',len(astr)
   print all, 'bytes   :',storage_size(astr)/8

   ! SHOULD WRITE INTO ASCII CREATE RAW UTF8 OR WHAT?
   print all, 'UCS4 characters written to ASCII internal file'
   aline=repeat(' ',len(ustr))
   write(aline,all)ustr
   aline=trim(aline)
   print all, 'length        :',len(aline)
   print all, 'bytes         :',storage_size(aline)/8
   print all,aline

   ! SHOULD WRITE INTO UCS4 DO ANYTHING DIFFERENT?
   print all
   print all, 'UCS4 characters written to UCS4 internal file'
   uline=repeat(' ',len(ustr))
   write(uline,all)ustr
   uline=trim(uline)
   print all, 'length        :',len(uline)
   print all, 'bytes         :',storage_size(uline)/8
   print all,uline

   ! SHOULD WRITE INTO ASCII CREATE RAW UTF8 OR WHAT?
   print all, 'ASCII characters into ASCII internal file'
   aline=repeat(' ',len(astr))
   write(aline,all)astr
   aline=trim(aline)
   print all, 'length        :',len(aline)
   print all, 'bytes         :',storage_size(aline)/8
   print all,aline

   ! SHOULD WRITE INTO UCS4 DO ANYTHING DIFFERENT?
   print all
   print all, 'ASCII characters into UCS4 internal file'
   uline=repeat(' ',len(astr))
   write(uline,all)astr
   uline=trim(uline)
   print all, 'length        :',len(uline)
   print all, 'bytes         :',storage_size(uline)/8
   print all,uline
   write(stdout,all)(ichar(uline(i:i)),",",i=1,len(uline))
   print all, 'And back again'
   write(stdout,all)'before',astr
   write(stdout,all)(ichar(astr(i:i)),",",i=1,len(astr))
   read(uline,'(a)')astr
   write(stdout,all)'after',astr
   write(stdout,all)(ichar(astr(i:i)),",",i=1,len(astr))

end program internal_io
```
## Summary

  Internal READ and WRITE statements are not a mechanism for converting
  between UTF-8 and UCS-4 character encodings like external files
  can be. There is no equivalent of the OPEN() statement option
  "ENCODING='UTF-8" for internal files. 

  This might seem surprising because
  internal I/O is used to encode and decode numeric values to and from 
  CHARACTER variables, and external files are the singular standard method
  for converting between interal UCS-4 representation and UTF-8 files.So 
  it would seem like internal I/O is the natural forum for character kind
  conversion, but that is __not__ the case.

  The best pracice is **like-with-like**. That is 

    + Do not read or write UCS-4 variables from an internal file of  DEFAULT
      or ASCII kind.

    + To use internal I/O with UCS-4 characters read and write from a
      character variable of UCS-4 kind.  You may also use ASCII characters
      with ADE(ASCII Decimal Equivalent) values from 0 to 128, but unlike 
      when using a default or ASCII internal file use of the unsiged values 129 to
      256 (aka. -128 to -1) is undefined.

  Note in a related manner that the "A" file descriptor produces a
  binary transfer of bytes for integers and reals and everything else
  until Unicode was introduced. UCS-4-kind characters are converted to
  and from UTF-8 when using external files open with ENCODING='utf-8',
  which breaks with the use of the "A" format field desciptor to transfer
  data of all types byte-by-byte to formatted files.

  So basically you cannot do much new with internal I/O accept read and write
  UCS-4 values in and out of UCS-4 internal files.
