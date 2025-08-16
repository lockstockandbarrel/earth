
**WIP Notice:** this document is an early "Work In Progress". I am still
sorting out what is and is not standard myself.

## Fortran Unicode Tutorial

Here are some lessons that describe my experiences with Unicode and
Fortran, including discussions concerning what is standardized and what
is not, what commonly-used extensions compilers provide to address some
of the current gaps, and what is known to be non-portable but useful
behavior from some compilers.

Once constructed and stable, the lessons will become a tutorial on
the Fortran Wiki, and this project will contain the related modules
and examples, but at this point this is just an incomplete outline so
starting the Wiki entries would be premature.

### Introduction to Fortran Unicode support
   + [**Lesson I:**](lesson1_ucs4.md) reading and writing UTF-8 Unicode files
   + [**Lesson II:**](lesson2_ucs4.md) creating Unicode strings in ASCII Fortran source files
   + [**Lesson III:**](lesson3_ucs4.md) mixing ASCII and UCS4 kinds as regards 
      + assignments
      + concatenation
      + passing arguments to external ASCII libraries
      + mixing kinds on I/O argument lists
   + [**Lesson IV:**](lesson4_ucs4.md)what is and is not supported with internal READ and WRITE statements
   + [**Lesson V:**](lesson5_ucs.md)processing Unicode file names on OPEN() statements
   + [**Lesson VI:**](lesson6_ucs4.md) reading UTF-8 strings from command lines
   + [**Lesson VII:**](lesson7_ucs4.md)  passing Unicode strings to and from C

### off the beaten path:
   + [**Lesson I:**](utf8_source_ext.md) UTF-8 source files -- just in comments and constants
   + [**Lesson II:**](backslash_ext.md) the backslash escape code extension
   + [**Lesson III:**](bom_ext.md) embedding BOM characters at the beginning of files

### Processing Unicode when ISO-10646 is not supported by a compiler
   + Lesson I: converting UTF-8 codes to and from INTEGER values
   + Lesson II: byte-oriented printing of 4-byte integers
   + Lesson III: issues with terminal emulators, system locale settings, and
                  other Unicode-related issues
   + Lesson IV: working with ASCII extended encodings; particularly those
                 commonly referred to as Extended, Latin, Latin1 and Latin2.

### The M_utf8 module
   + converting between UCS-4 and UTF-8 with procedures
   + conversion routines to bridge the gaps in Fortran Unicode processing
   + related utility programs
-------------------------------------------------------------
+ [PREVIOUS](https://github.com/lockstockandbarrel/earth)
+ [NEXT](https://github.com/lockstockandbarrel/earth/blob/main/docs/lesson1_ucs4.md)
