## Introduction to Fortran Unicode support
### Lesson II: creating Unicode strings in ASCII Fortran source files

Since ASCII is a subset of Unicode the line is a bit blurry as to what
encoding source files may use. Can an input file be UTF-8 encoded?
In particular, can constant strings and comments be composed in Unicode
or must the entire file be ASCII? What about extended ASCII, which uses
all 256 values representable in one byte, versus strict adherence to
the defined 128 ASCII characters or even the Fortran character set,
which is a subset of the ASCII characters?

Section 6.1(Processor character set) and 7.4.4(Character constants)
provide guidance on this. A lot is left up to the processor. A conservative
interpretation implies that for ASCII input files a quoted constant string
will represent bytes transferred directly to the variable or constant
value being defined. What is left in question is what encoding ensues
otherwise, when a file may be using one encoding like UTF-8, but a
constant using another encoding such as UCS-4 is being declared.

For example, currently if I edit a UTF-8 file and create
```fortran
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
```
## Output

```text
  OUTPUT  LEN    BYTES    KIND
    ?     1       1        1
    €     3       3        1
    ?     3       3        1
    ?     1       4        4
    ?     1       4        4
    €     1       4        4
```
We want to see a euro character, have a string with a length of 1
that is stored in four bytes, and be of kind UCS-4. So only UERO3
is a correctly generated value.

We are partly just demonstrating there are a lot of ways to specify
a string constant that will _not_ end up creating a proper UCS-4
string, but one (verbose) syntax that should always succeed.

## CHAR() Intrinsic:

So we have demonstrated The CHAR() intrinsic function can be used to
construct characters from their Unicode code points, allowing you to
reliably embed Unicode characters within character strings.

# Mixing CHAR() and quoted constants

A string literal like
```fortran
ucs4_'Unicode character: ' // char(9787, kind=ucs4)
```
demonstrates creating a Unicode string using the ucs4 kind for a quoted
string and the CHAR() intrinsic together. This too is standard.

## a program to convert a utf-8 file to Fortran CHAR() declarations
```fortran

Since typing all those code point values can get tedious, lets construct
a program that reads a UTF-8 file and converts it to a program that
defines all the input lines as UCS-4 variables:

program unifile_to_ftn
! @(#) take command line argument utf-8 text and generate Fortran statement that represents the string as char(3f) calls
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter             :: form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter             :: g= '(*(g0))'
character(len=80)                      :: count
integer                                :: i, j, iostat
character(len=4096,kind=ucs4)          :: uline
   open (stdin, encoding='UTF-8')
   open (stdout, encoding='UTF-8')
   write(stdout,g) 'program testit'
   write(stdout,g) 'use, intrinsic :: iso_fortran_env, only : output_unit'
   write(stdout,g) "integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')"
   write(stdout,g) "   open (output_unit, encoding='utf-8')"
   do j=1,huge(0)-1
      read(stdin,'(a)',iostat=iostat)uline
      if(iostat.ne.0)exit
      write(count,g) "variable_",j,"= &"
      write(stdout,g) 'block'
      write(stdout,g) '! Unicode code points for ',trim(uline)
      write(stdout,g) 'character(len=*,kind=ucs4),parameter :: '//trim(count)
      write(stdout,form)(uline(i:i),i=1,len_trim(uline))
      write(stdout,g) "   write(output_unit,'(a)' )variable_",j
      write(stdout,g) 'endblock'
   enddo
   write(stdout,g) "end program testit"
end program unifile_to_ftn
```
### Input
```text
七転び八起き。
転んでもまた立ち上がる。
くじけずに前を向いて歩いていこう。
```
### Output

It should be relatively easy to copy and paste and edit the resulting
variable declarations into source files where it is needed. The output
is generated as a complete program that should reproduce the input file
(sans any trailing spaces) when executed.

```fortran
program testit
use, intrinsic :: iso_fortran_env, only : output_unit
integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')
   open (output_unit, encoding='utf-8')
block
! Unicode code points for 七転び八起き。
character(len=*,kind=ucs4),parameter :: variable_1= &
char(int(z'4E03'),kind=ucs4)// &
char(int(z'8EE2'),kind=ucs4)// &
char(int(z'3073'),kind=ucs4)// &
char(int(z'516B'),kind=ucs4)// &
char(int(z'8D77'),kind=ucs4)// &
char(int(z'304D'),kind=ucs4)// &
char(int(z'3002'),kind=ucs4)
   write(output_unit,'(a)' )variable_1
endblock
block
! Unicode code points for 転んでもまた立ち上がる。
character(len=*,kind=ucs4),parameter :: variable_2= &
char(int(z'8EE2'),kind=ucs4)// &
char(int(z'3093'),kind=ucs4)// &
char(int(z'3067'),kind=ucs4)// &
char(int(z'3082'),kind=ucs4)// &
char(int(z'307E'),kind=ucs4)// &
char(int(z'305F'),kind=ucs4)// &
char(int(z'7ACB'),kind=ucs4)// &
char(int(z'3061'),kind=ucs4)// &
char(int(z'4E0A'),kind=ucs4)// &
char(int(z'304C'),kind=ucs4)// &
char(int(z'308B'),kind=ucs4)// &
char(int(z'3002'),kind=ucs4)
   write(output_unit,'(a)' )variable_2
endblock
block
! Unicode code points for くじけずに前を向いて歩いていこう。
character(len=*,kind=ucs4),parameter :: variable_3= &
char(int(z'304F'),kind=ucs4)// &
char(int(z'3058'),kind=ucs4)// &
char(int(z'3051'),kind=ucs4)// &
char(int(z'305A'),kind=ucs4)// &
char(int(z'306B'),kind=ucs4)// &
char(int(z'524D'),kind=ucs4)// &
char(int(z'3092'),kind=ucs4)// &
char(int(z'5411'),kind=ucs4)// &
char(int(z'3044'),kind=ucs4)// &
char(int(z'3066'),kind=ucs4)// &
char(int(z'6B69'),kind=ucs4)// &
char(int(z'3044'),kind=ucs4)// &
char(int(z'3066'),kind=ucs4)// &
char(int(z'3044'),kind=ucs4)// &
char(int(z'3053'),kind=ucs4)// &
char(int(z'3046'),kind=ucs4)// &
char(int(z'3002'),kind=ucs4)
   write(output_unit,'(a)' )variable_3
endblock
end program testit
```
## Escape Sequences:

Several compilers also allow for the quoted strings to contain code point
escape sequences. This is not standard and the syntax may therefore vary
from processor to processor. For example, gfortran(1) has the -fbackslash
compiler option:
```text
    -fbackslash

        "\x"nn, "\u"nnnn and "\U"nnnnnnnn (where each n is a hexadecimal
        digit) are translated into the Unicode characters corresponding to
        the specified code points.

## Other characters with regards to the Fortran standard

Section 6.1.6 of the f2023 standard says

        Additional characters _(those not in the Fortran character set)_
        may be representable in the processor, but shall appear only
        in comments (6.3.2.3, 6.3.3.2), character constants (7.4.4),
        input/output records (12.2.2), and character string edit
        descriptors (13.3.2).


In a later lesson we will see how we can take advantage of that statement
to create a what-you-see-is-what-you-get behavior that with todays'
processors and operating systems should be very portable as well.

## Summary

That is it for this lesson. As tempting as it may be to place Unicode
multi-byte characters in quoted constant strings in code source, the
portable standard method is to use the CHAR() function and Unicode code
point values.

Placing messages in an external file and opening the file as UTF-8 encoded
is an easy alternative that lets you maintain the messages as Unicode
directly, but this will require always making the message file accessible
when the program is being used.

