## Introduction to Fortran Unicode support
### Lesson IV:  what is and is not supported with internal READ and WRITE statements

   There are not many new facets to Fortran provided by Unicode support regarding
   internal I/O, but there are a few nuances that need heeded.

   From the 2023 Fortran standard, regarding internal I/O:

      Fortran 90 permitted deﬁned assignment between character strings
      of the same rank and different kinds.
   
      This document does not permit that if both of the different kinds are
      ASCII, ISO 10646, or default kind.  An input/output list shall
      not contain an effective item that is non-default character except
      for ISO 10646 or ASCII character if the data transfer statement
      specifies an internal file of ISO 10646 character kind.
   
      An input/output list shall not contain an effective item of type
      character of any kind other than ASCII if the data transfer statement
      speciﬁes an ASCII character internal file.
   
      An output list shall not contain an effective item that is a
      boz-literal-constant.

   One might expect you could do encoding to and from UTF-8 and UCS-4
   with an internal file as you can with an external file. Internal
   files are the standard method for converting numeric values to and
   from string variables, making is seem even more natural to expect 
   internal I/O to provide some type of conversions between encodings.

   __There is no such automatic conversion between CHARACTER kinds 
   when using internal files!__

   You would probably also expect the "A" format field descriptor would
   produce a binary copy of bytes when the type is CHARACTER as it does
   with other types; whether the internal file is UCS4 or ASCII. The
   "A" field descriptor is the original equivalent of the TRANSFER()
   function with added field descriptor control. That was the most common
   method for allowing binary data to be placed in formatted files prior
   to stream I/O support, for example.

   __Yes, but only for CHARACTER variables of the same kind__. Using
   other CHARACTER kinds that do not match the internal file kind are
   not even allowed.

   The second paragraph quoted from the standard above makes it seem
   that perhaps you can write ASCII bytes into a ISO 10646 variable,
   but it is unclear whether that would be a byte-per-byte transfer or
   whether only the 128 ASCII characters would be allowed or whether
   it might treat the data as UTF-8 and encode it into UCS4. In all
   compilers I have tried the ASCII writes into a UCS4 variable fail
   accept for ADE values from 0 o 128.

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
   write(stdout,all)'before:',astr
   write(stdout,all)(ichar(astr(i:i)),",",i=1,len(astr))
   read(uline,'(a)')astr
   write(stdout,all)'after:',astr
   write(stdout,all)(ichar(astr(i:i)),",",i=1,len(astr))

end program internal_io
```
### Output
```text
unicode UCS-4 string
 Hello World and Ni Hao -- ä½ å¥½
length  :28
bytes   :112
ASCII bytes
 Hello World and Ni Hao -- ä½ å¥½
length  :32
bytes   :32
UCS4 characters written to ASCII internal file
length        :28
bytes         :28
Hello World and Ni Hao -- ??

UCS4 characters written to UCS4 internal file
length        :28
bytes         :112
Hello World and Ni Hao -- ä½ å¥½
ASCII characters into ASCII internal file
length        :32
bytes         :32
Hello World and Ni Hao -- ä½ å¥½

ASCII characters into UCS4 internal file
length        :32
bytes         :128
Hello World and Ni Hao -- ¿¿¿¿¿¿¿¿¿¿¿¿¿¿
72,101,108,108,111,32,87,111,114,108,100,32,97,110,100,32,78,105,32,72,97,111,32,45,45,32,-28,-67,-96,-27,-91,-67,
And back again
before:Hello World and Ni Hao -- ä½ å¥½
72,101,108,108,111,32,87,111,114,108,100,32,97,110,100,32,78,105,32,72,97,111,32,45,45,32,228,189,160,229,165,189,
after:Hello World and Ni Hao -- ??????
72,101,108,108,111,32,87,111,114,108,100,32,97,110,100,32,78,105,32,72,97,111,32,45,45,32,63,63,63,63,63,63,
```
## Summary

  Internal READ and WRITE statements are not a mechanism for converting
  between UTF-8 and UCS-4 character encodings like external files
  can be. There is no equivalent of the OPEN() statement option
  "ENCODING='UTF-8" for internal files.

  This might seem surprising because internal I/O is used to encode and
  decode numeric values to and from CHARACTER variables, and external
  files are the singular standard method for converting between internal
  UCS-4 representation and UTF-8 files.So it would seem like internal
  I/O is the natural forum for character kind conversion, but that is
  __not__ the case.

  The best practice is **like-with-like**. That is 

    + Do not read or write UCS-4 variables from an internal file of
      DEFAULT or ASCII kind.

    + To use internal I/O with UCS-4 characters read and write from a
      character variable of UCS-4 kind.  You may also read and write ASCII
      characters with ADE(ASCII Decimal Equivalent) values from 0 to 128,
      but unlike when using a default or ASCII internal file use of the
      unsigned values 129 to 256 (aka. integer byte values -128 to -1)
      is undefined.

  Note in a related manner that the "A" file descriptor produces a
  binary transfer of bytes for integers and reals and everything else
  until Unicode was introduced. UCS-4-kind characters are converted to
  and from UTF-8 when using external files open with ENCODING='utf-8',
  which breaks with the use of the "A" format field descriptor to transfer
  data of all types byte-by-byte to formatted files.

  So basically with Unicode support added you cannot do much new with
  internal I/O accept read and write UCS-4 values in and out of UCS-4
  internal files.

+ [NEXT](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson2_ucs5.md)
+ [PREVIOUS](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson3_ucs4.md)
