## Introduction to Fortran unicode support

Not all Fortran compilers provide high-level ISO-10646 (ie. "unicode")
support. To determine if a compiler provides support, one can attempt
to compile and execute the following program:

```fortran
   program test_for_iso_10646
   use iso_fortran_env, only : selected_char_kind, output_unit
   implicit none
   intrinsic selected_char_kind
   integer, parameter :: ucs4 = selected_char_kind ('ISO_10646')
      open(output_unit,encoding='utf-8')
      write(*,*)'Smiling face with open mouth',char(int(z'1F603'),kind=ucs4) ! 😃
   end program test_for_iso_10646
```

If this program builds and runs and prints an emoji the compiler
provides support. If not, unicode usage will be indirect, and require
lower-level knowledge of byte-level Fortran processing and I/O and the
hosting operating system. This introduction only applies to compilers
providing ISO-10646 support.

Unicode support is straight-forward if the program itself does not need
to contain unicode strings. Assuming the program merely needs to read
or write UTF-8 files and at most use Fortran intrinsics to operate on
the strings there is very little different from processing ASCII files.

To nearly transparently process Unicode UTF-8 files
declare all your character variables to be kind "iso_10646", and
open/reopen your files with UTF-8 encoding, being careful to ensure your
intrinsic calls are using unicode.

Fortran will transparently convert the data from UTF-8 to whichever
unicode encoding it uses internally (UTF-8, UTF-32, UTF-16, ...) on input,
and convert back to UTF-8 on output.

If standard-conforming, the internal representation will be UCS-4, as the
standard description of the intrinsic SELECTED_CHAR_KIND() states:

   If NAME has the value ISO_10646, then the result has a value equal
   to that of the kind type parameter of the ISO 10646 character kind
   (corresponding to UCS-4 as speciﬁed in ISO/IEC 10646) if the
   processor supports such a kind; otherwise the result has the value
   −1.

It is assumed here that ISO_10646 implies standard-conforming UCS-4
internal representation. At this level of discussion what matters is that
you know the memory required to hold the characters will be four times
greater than if they were ASCII characters, as all UCS-4 characters are
4 byte values.

Many useful programs adhere to these restrictions. A simplistic example
that reads a UTF-8 file with lines up to 4096 bytes and outputs the
file prefixing each line with a glyph/character count:
```fortran
program count_glyphs
! @(#) read a utf-8 file and write it out with lines prefixed with glyph count of the line
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit, stdin=>input_unit
implicit none
intrinsic selected_char_kind
intrinsic is_iostat_end
intrinsic len_trim
integer, parameter            :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter    :: g= '(*(g0))'
integer                       :: length
integer                       :: i
integer                       :: iostat
character(len=4096,kind=ucs4) :: uline ! specifies maximum line length of 4096 bytes,
                                       ! which might be as few as 1024 (ie. 4096/4) glyphs
character(len=255)            :: iomsg

   open (stdin, encoding='UTF-8')
   open (stdout, encoding='UTF-8')

   do
      read(stdin,'(a)',iostat=iostat,iomsg=iomsg)uline
      if(iostat.eq.0)then
         length=len_trim(uline)
         write(stdout,'(i9,": ",a)')length,uline(:length)
      elseif(is_iostat_end(iostat))then
         exit
      else
         ! does the ASCII message have to be converted to ucs-4?
         ! this will be discussed in detail later, but for now
         ! remember you can change the encoding of a file dynamically
         open (stdout, encoding='ascii')
         write(stdout,g)'<ERROR>',trim(iomsg)
         exit
      endif
   enddo

end program count_glyphs
```
The data will be converted from UTF-8 to UCS-4 and back again
transparently.

So if we create a file called "upagain.utf"
```text
七転び八起き。
転んでもまた立ち上がる。
くじけずに前を向いて歩いていこう。
```
and make sure that our terminal displays UTF-8 files properly
by displaying that file to the screen, then runnng the program
```bash
./count_glyphs < upagain.utf
```
should produce
```text
```
############################

The oddities are encountered when reading from an internal file
(case they only be ASCII?) or when declaring a constant with a quoted
string.

ASCII technically is only 128 different characters, but default character
sets are almost always able to work with values from 0 o 255, which allows
entering what looks like UFT-8 characters in a string in the code.

So fortran can work with stream I/O and UTF-8 files as bytes of characters
relatively easily even without UTF=8 support( which is optional). But for
string slicing and the fortran character intrinsics to work the gfortran
compiler (currently, at least) requires the characters to be represented
as 4-byte values.

   I found that there’s very little information around on how to use
   Unicode characters in Fortran so I did a small writeup covering my
   experiences with the topic. The examples are available here if anyone
   want’s to try them out: [7]GitHub - plevold/unicode-in-fortran

Introduction

     * WARNING
       The following examples are based on my own experiences and
       testing.I’m neither a Unicode expert nor a compiler maintainer. If
       you find anything wrong with the examples please open an [8]issue.

   Using Unicode characters in you programs is not necessarily hard. There
   is however very little information about Fortran and Unicode available.
   This repository is a collection of examples and some explanations on
   how to use Unicode in Fortran.

   Most of what is written here is based on recommendations from the
   [9]UTF-8 Everywhere Manifesto. I would highly recommend that you read
   that as well to get a better understanding of what Unicode is and is
   not.

Compilers

   The examples used here have been verified to work on the following
   compiler/OS combinations:
   Compiler Version  Operating System Status
   gfortran 9.3.0    Linux            :white_check_mark:
            10.3.0   Windows 10       :white_check_mark:
   ifort    2021.5.0 Linux            :white_check_mark:

Creating and Printing Unicode Strings

   First, make sure that
     * Your terminal emulator is set to UTF-8.
     * Your source file encoding is set to UTF-8.

   With the notable exception of Windows CMD and PowerShell, UTF-8 is
   robably the default encoding in your terminal. If you’re using Windows
   CMD or PowerShell you need to use a modern terminal emulator like
   [10]Windows Terminal and follow the instructions [11]here. If that’s
   too much hassle you can consider switching to [12]Git for Windows
   instead which will give you a nice Bash terminal on Windows.

   With that in place insert unicode characters directly into a string
   literal in your source code. If you’re using Visual Studio Code there’s
   an [13]extension that can help you with inserting Unicode characters in
   your source files. Using escape sequences like \u1F525 requires setting
   special compiler flags and different compilers seems to handle this
   somewhat differently. Unless you know for sure that you want to stick
   with one compiler forever I would not recommend doing this.

   If you’re storing it in a variable, use the default character kind or
   c_char form iso_c_binding. Do not try to use e.g.
   selected_char_kind('ISO_10646') to create “wide” (longer than one byte)
   character elements. For one thing, Intel Fortran does as of this
   writing not support this. Also if you’re going to pass character
   arguments to procedures you’ll either have to do conversion between the
   default and the ISO_10646 character kinds or you need to have two
   versions of each procedure that might need to accept both wide and
   default character kinds. As we will later see, this is never really
   needed so you will
   only create extra work for yourself.

   Example:
program write_to_console
    implicit none
    character(len=:), allocatable :: chars

    chars = 'Fortran is 💪, 😎, 🔥!'
    write(*,*) chars
end program

   This should output
❯ fpm run --example write_to_console
 Fortran is 💪, 😎, 🔥!

   As we can see from in output from the example above the emojis are
   printed like we inserted them in the source file.

Determining the Length of a Unicode String

   Some might be confused by that
program unicode_len
    implicit none
    character(len=:), allocatable :: chars

    chars = 'Fortran is 💪, 😎, 🔥!'
    write(*,*) len(chars)
    if (len(chars) /= 28) error stop
end program

   outputs
❯ fpm run --example unicode_len
          28

   while if we manually count the number of character we see in the string
   literal then we end up 19 character. This is because in Unicode what we
   perceive as one character might consist of multiple bytes. This is
   referred to as a grapheme cluster and is crucial when rendering text.
   Determining the number of grapheme clusters and their width when
   rendered on the screen is a complex task which we will not go into
   here. For more information see the [14]UTF-8 Everywhere Manifesto and
   [15]It’s Not Wrong that “ :man_facepalming:t3: ”.length == 7.

   We’re mainly concerned about storing the characters in memory though,
   as our terminal emulator or text editor takes care of displaying the
   results on our screen. For this it is useful to think of the character
   variable as a sequence of bytes rather than a sequence of what we
   perceive as one character. When len(chars) == 28 that means that we
   need 28 elements in our variable to store the string.

Searching for Substrings

   Substrings can be searched for using the regular index intrinsic just
   like strings with just ASCII characters:
program unicode_index
    implicit none
    character(len=:), allocatable :: chars
    integer :: i

    chars = '📐: 4.0·tan⁻¹(1.0) = π'
    i = index(chars, 'n')
    write(*,*) i, chars(i:i)
    if (i /= 14) error stop
    i = index(chars, '¹')
    if (i /= 18) error stop
    write(*,*) i, chars(i:i + len('¹') - 1)
end program

   outputs
❯ fpm run --example unicode_index
          14 n
          18 ¹

   There is no need for any special handling thanks to the design of
   Unicode:

     Also, you can search for a non-ASCII, UTF-8 encoded substring in a
     UTF-8 string as if it was a plain byte array — there is no need to
     mind code point boundaries. This is thanks to another design feature
     of UTF-8 — a leading byte of an encoded code point can never hold
     value corresponding to one of trailing bytes of any other code
     point.
     — [16]UTF-8 Everywhere Manifesto

   Keep in mind though that what looks like a single character (a grapheme
   cluster) might be more than one byte long so chars(i:i) will not
   necessarily output the complete match.

Reading and Writing to File

   Reading and writing Unicode characters from and to a file is as easy as
   writing ASCII text:
program file_io
    implicit none

    ! Write to file
    block
        character(len=:), allocatable :: chars
        integer :: unit

        chars = 'Fortran is 💪, 😎, 🔥!'
        open(newunit=unit, file='file.txt')
        write(unit, '(a)') chars
        write(*, '(a)') ' Wrote line to file: "' // chars // '"'
        close(unit)
    end block

    ! Read back from the file
    block
        character(len=100) :: chars
        integer :: unit

        open(newunit=unit, file='file.txt', action='read')
        read(unit, '(a)') chars
        write(*,'(a)') 'Read line from file: "' // trim(chars) // '"'
        close(unit)
        if (trim(chars) /= 'Fortran is 💪, 😎, 🔥!') error stop
    end block

end program

   The open statement in Fortran allows to one to specify
   encoding='UTF-8'. In testing with ifort and gfortran however this does
   not seem to have any impact on the file written. Specifying encoding
   does for example not seem to add a [17]Byte Order Mark (BOM) neither
   with gfortran nor ifort.

Conclusion

   We’ve seen that using Unicode characters in Fortran is actually not
   that hard! One need to remember that what we perceive as a character is
   not necessarily a single element in our character variables. Apart from
   that using Unicode characters in Fortran should really be quite
   straight forward.

   18 Likes

   [18]Handling files in directories whose names are not entirely ASCII

   [19]How to use utf-8 in gfortran?

   [20]Arjen February 10, 2022, 7:27pm 2

   Thanks for this write-up. The principles of UNICODE are indeed not that
   hard, but there are some very nasty areas, like surrogate pairs and
   characters to change the direction of reading, where it gets ugly. (I
   have read a paper about the latter where the authors demonstrated that
   you could use such direction changes to hide the actual source code).
   As for BOMs, that seems to be a typical Windows thing.

   2 Likes

   [21]jacobwilliams February 11, 2022, 4:07am 3

   I’m not sure that some of this is good advice. It seems what you are
   doing here is akin to stuffing double precision reals into an array of
   single precision reals… It sort of works under some circumstances, but
   I wouldn’t recommend it. I think using the
   selected_char_kind('ISO_10646') is the correct way. See my
   [22]JSON-Fortran library, which does support unicode. And yes, it isn’t
   currently supported by ifort (what gives, Intel?), and yes, you have to
   write multiple versions of routines (but that’s the same way you have
   to do for different real kinds, so it is not unexpected).

   Consider this file (‘unicode.txt’):

   :grinning: :sunglasses: :weary:

   And the following code:
program test

use iso_fortran_env

implicit none

integer,parameter :: CK = selected_char_kind('ISO_10646')

character(kind=CK,len=3) :: s
integer :: iunit

open(output_unit,encoding='utf-8')

open(newunit=iunit,file='unicode.txt',status='OLD',encoding='UTF-8')

read(iunit,'(A)') s

write(output_unit,*) s
write(output_unit,*) 'len(s) = ', len(s)
write(output_unit,*) 's(1:1) = ', s(1:1)

end program test

   This prints:
😀😎😩
 len(s) =            3
 s(1:1) = 😀

   So, notice how the length is 3 and the slicing works correctly.

   But, I don’t think Fortran actually supports unicode in source files.
   For example, when I try to do this:
s = CK_'😀😎😩'

   I get the warning “CHARACTER expression will be truncated in assignment
   (3/12) at (1) [-Wcharacter-truncation]” and s(1:1) will print as
   gibberish.

   3 Likes

   [23]FortranFan February 11, 2022, 5:12am 4

   jacobwilliams:

     I think using the selected_char_kind('ISO_10646') is the correct
     way. See my [24]JSON-Fortran library, which does support unicode.
     And yes, it isn’t currently supported by ifort (what gives, Intel?),

   Attention [25]@greenrongreen - please see above. Any particular hurdle
   that prevents IFORT from supporting the wider character set? The
   standard acknowledged ISO 10646 back with the Fortran 2003 revision and
   enabled a mechanism to support it: IFORT has long “claimed” Fortran
   2003 compliance and this is a common enough need among the users that
   its absence is felt with IFORT.

   2 Likes

   [26]plevold February 11, 2022, 5:53am 5

   Thanks for the feedback [27]@jacobwilliams. If your interested in this
   topic I’d highly recommend to read the [28]UTF-8 Everywhere Manifesto
   which covers this in much more detail than what I did. Here’s a
   relevant quote from their conclusion:

     In particular, we believe that adding wchar_t to the C++ standard
     was a mistake, and so are the Unicode additions to C++11. What must
     be demanded from the implementations though, is that the basic
     execution character set would be capable of storing any Unicode
     data. Then, every std::string or char* parameter would be
     Unicode-compatible. ‘If this accepts text, it should be Unicode
     compatible’—and with UTF-8, it is easy to achieve.

   selected_char_kind('ISO_10646') in Fortran would be similar to wchar_t
   in C++.

   FortranFan:

     Any particular hurdle that prevents IFORT from supporting the wider
     character set?

   Only speculation from my side, but perhaps Intel has acknowledged the
   issue with wide character types and because of this don’t bothered
   implementing support for it?

   [29]drikosev February 11, 2022, 6:51am 6

   plevold:

     I found that there’s very little information around on how to use
     Unicode characters in Fortran so I did a small writeup covering my
     experiences with the topic.

   Hello,

   This issue was raised also some time ago in c.l.f (with some current
   responders).
   As wisely mentioned ie by jacobwilliams, the length of a UTF-8 string
   is not
   inherently supported. See ie the user defined ‘ulen’ function in my
   gist:
   https:// [30]BOZ Literal Constants in function "ulen" · GitHub

   Another issue for Fortraners is that the 2003 standard acknowledged ISO
   10646.
   Also one of the responders had informed me that my example wasn’t very
   portable.

   Not sure how difficult would be for Fortran implementers to support
   transparently
   a UTF-8 aware LEN intrinsic that would simplify a lot the current
   situation.

   Ev. Drikos

   1 Like

   [31]plevold February 11, 2022, 7:48am 7

   I’m not quite sure I understood what your ulen function is trying to
   achieve. Do you want to count the number of bytes in the character
   sequence, the number of grapheme clusters or the width of the text
   displayed on screen?

   The number of bytes can be computed easily with len(chars) (multiplied
   by a constant if using non-default character kinds).

   Counting the number of grapheme clusters is not that straight forward,
   but can be done if you find the right algorithm and port it to Fortran
   or make a C interface . I think for example the Rust crate
   [32]unicode-segmentation will do that for you.

   Determining the width of a string boils down to determining the width
   of each grapheme cluster. Even for monospaced fonts this is a
   non-trivial task. Take for example the following string:
|😎|⋮|

   The characters are ([33]here is a nice tool to determine that):
U+007C : VERTICAL LINE {vertical bar, pipe}
U+1F60E : SMILING FACE WITH SUNGLASSES
U+007C : VERTICAL LINE {vertical bar, pipe}
U+22EE : VERTICAL ELLIPSIS
U+007C : VERTICAL LINE {vertical bar, pipe}

   If we try to align this with punctuation marks
|😎|
|...|
|⋮|
|.|

   we see that even for a monospaced font
     * The SMILING FACE WITH SUNGLASSES emoji is slightly shorter that
       three punctuation marks
     * The VERTICAL ELLIPSIS is slightly shorter than one punctuation mark

   This is even further complicated by the fact that if the monospace font
   in use does not have a character the application or (most likely) the
   OS will fallback to another font. Because of this it might even be that
   you’re seeing a different width of the characters above than what I’m
   doing!

   [34]drikosev February 11, 2022, 7:56am 8

   plevold:

     I’m not quite sure I understood what your ulen function is trying to
     acheive.

   It counts the number of characters represented, which is what the LEN
   intrinsic does.

   Ev. Drikos

   [35]plevold February 11, 2022, 8:44am 9

   Ok, so I assume your trying to count the number of grapheme clusters
   then.

   I tried to extract the ulen-function and apply it to a Fortran
   character string, but I think I’m doing something wrong. At least I’m
   not able to produce any meaningful output with it. Any thoughts?
module ulen_mod
    implicit none

    public ulen

contains

    integer function ulen(chars)
        character(len=*), intent(in) :: chars

        integer :: i

        ulen = 0
        do i = 1, len(chars)
            ulen = ulen + ulen_single(chars(i:i))
        end do
    end function


    function ulen_single(ch) result(ulen)
        use iso_c_binding, only: c_char
        implicit none

        character(kind=c_char), intent(in) ::ch
        integer :: ulen, ich

        ulen=0
        ich = ichar(ch)

        if ( ich < int(Z'80') ) THEN
            ulen=1
        else if ( (ich > ( int(Z'C0') + 1)) .and. ( ich < int(Z'E0') )) THEN
            ulen=2
        else if ( ich < int(Z'F0') ) THEN
            ulen=3
        else if ( ich <= int(Z'F4') ) THEN
            ulen=4
        else
            ulen=1   !assume we process larger sequqences, 1 by 1 bytes
        end if


    end function
end module

program main
    use ulen_mod, only: ulen

    write(*,*) ulen('abc'), len('abc') ! 3 grapgheme clusters
    write(*,*) ulen('😎'), len('😎') ! 1 grapheme cluster
    write(*,*) ulen('a̐éö̲'), len('a̐éö̲') ! 3 grapheme clusters
end program

   [36]vmagnin February 11, 2022, 9:20am 10

   jacobwilliams:

     And yes, it isn’t currently supported by ifort (what gives, Intel?)

   Neither by ifx 2022.0.0 :frowning:

   [37]drikosev February 11, 2022, 10:38am 11

   Admittedly, my reply wasn’t complying with the documentation of the
   ‘ulen’ function, which counts the number of bytes. So, in your program
   the ‘ulen’ function could be something like that:
integer function ulen(chars)
    character(len=*), intent(in) :: chars
    integer :: i, j, bytes

    ulen = 0
    bytes= len(chars)
    if ( bytes == 0 ) then
        ulen = 0
        return
    end if
    i = 1
    do, while ( i <= bytes )
           j = ulen_single(chars(i:i))
        i = i + j
        ulen = ulen + 1
    end do
end function

   The results are displayed below
       3           3
       1           4
       5           9

   The following Bash script ie prints the same results for the last
   string in your program:

   #!/bin/bash
   string_variable_name=‘a̐éö̲’
   charlen=${#string_variable_name}
   echo $charlen

   [38]plevold February 11, 2022, 11:03am 12

   Thanks! That makes more sense than my naive attempt :slight_smile:

   With the updated example:
module ulen_mod
    implicit none

    public ulen

contains

    integer function ulen(chars)
        character(len=*), intent(in) :: chars
        integer :: i, j, bytes

        ulen = 0
        bytes= len(chars)
        if ( bytes == 0 ) then
            ulen = 0
            return
        end if
        i = 1
        do, while ( i <= bytes )
            j = ulen_single(chars(i:i))
            i = i + j
            ulen = ulen + 1
        end do
    end function


    function ulen_single(ch) result(ulen)
        use iso_c_binding, only: c_char
        implicit none

        character(kind=c_char), intent(in) ::ch
        integer :: ulen, ich

        ulen=0
        ich = ichar(ch)

        if ( ich < int(Z'80') ) THEN
            ulen=1
        else if ( (ich > ( int(Z'C0') + 1)) .and. ( ich < int(Z'E0') )) THEN
            ulen=2
        else if ( ich < int(Z'F0') ) THEN
            ulen=3
        else if ( ich <= int(Z'F4') ) THEN
            ulen=4
        else
            ulen=1   !assume we process larger sequqences, 1 by 1 bytes
        end if


    end function
end module

program main
    use ulen_mod, only: ulen

    write(*,*) ulen('abc'), len('abc') ! 3 grapgheme clusters
    write(*,*) ulen('😎'), len('😎') ! 1 grapheme cluster
    write(*,*) ulen('a̐éö̲'), len('a̐éö̲') ! 3 grapheme clusters
end program

   I get:
           3           3
           1           4
           7          11

   So for the last string we get different results which is odd. The
   answer is wrong in both cases though as there’s 3 and not 5 or 7
   grapheme clusters. The example was taken from the
   [39]unicode-segmentation docs which correctly splits it into 3 parts
   (admittedly I haven’t verified this myself).

   I wonder if there’s ever any need for doing these calculations in
   Fortran though?

   What I think would be interesting in some cases is to compute the width
   of a string so that output can be aligned when using a monospaced font
   e.g. in a terminal. As I [40]previously mentioned this is a very
   challenging task so I don’t know how feasible it is.

   [41]drikosev February 11, 2022, 11:24am 13

   plevold:

     So for the last string we get different results which is odd.
     The answer is wrong in both cases though as there’s 3 and not 5 or 7
     grapheme clusters.

   Thanks, I switched from Firefox to Safari and now I see ‘7 11’ in the
   last line, also ‘7’ by the Bash script.
   Note that I never spoke about ‘grapheme clusters’, only you did it,
   which of course may be what a user can have in mind. Sample code in my
   gists wouldn’t go that far. It counts only valid UTF-8 sequences that
   Unicode characters consists of, and in fact I’ve restricted it up to 4
   bytes (just skipped longer ones).

   Ev. Drikos

   [42]plevold February 11, 2022, 11:55am 14

   The problem is that, at least as far as I understand, a “character” is
   a very vague term and not precisely defined in Unicode. I’ll highly
   reccoment the [43]Characters section of the UTF-8 Everywhere Manifesto.
   Some relevant quotes:

     (…)
     * User-perceived character — Whatever the end user thinks of as a
       character. This notion is language dependent. For instance, ‘ch’ is
       two letters in English and Latin, but considered to be one letter
       in Czech and Slovak.
     * Grapheme cluster — A sequence of coded characters that ‘should be
       kept together’.[§2.11] Grapheme clusters approximate the notion of
       user-perceived characters in a language independent way. They are
       used for, e.g., cursor movement and selection.
       (…)

     ‘Character’ may refer to any of the above. The Unicode Standard uses
     it as a synonym for coded character.[§3.4] When a programming
     language or a library documentation says ‘character’, it typically
     means a code unit. When an end user is asked about the number of
     characters in a string, he will count the user-perceived characters.
     A programmer might count characters as code units, code points, or
     grapheme clusters, according to the level of the programmer’s
     Unicode expertise. For example, [44]this is how Twitter counts
     characters. In our opinion, a string length function should not
     necessarily return one for the string ‘ :koala: ’ to be considered
     Unicode-compliant.

   (Emphasis on the last sentence added by me)

   [45]Culture setting / inoculation against squiggles

   [46]drikosev February 11, 2022, 12:41pm 15

   Just for the record, in Java with the JDK 1.8 I see 7 characters. Also,
   with this Fortran code (gfortran), I see 7 characters as demonstrated
   below. But I guess it could also consist of 5 characters only if I’d
   used the precomposed ‘é’ and ‘ö’.
program test
    use iso_fortran_env
    implicit none

    integer,parameter :: CK = selected_char_kind('ISO_10646')

    !see also https://en.wikipedia.org/wiki/Combining_Diacritical_Marks
    !see also https://www.compart.com/en/unicode/U+006F
    character(kind=CK,len=7) :: s = CHAR(Z'0061', KIND=CK) //& !a
                                  & CHAR(Z'0310', KIND=CK) //& !◌̐ -> a̐
                                ! & CHAR(Z'00E9', KIND=CK) //& !é (precomposed)
                                  & CHAR(Z'0065', KIND=CK) //& !e (decomposed)
                                  & CHAR(Z'0301', KIND=CK) //& !◌́
                                ! & CHAR(Z'00F6', KIND=CK) //& !ö  (precomposed
)
                                  & CHAR(Z'006F', KIND=CK) //& !o (decomposed)
                                  & CHAR(Z'0308', KIND=CK) //& !Combining Diaere
sis
                                  & CHAR(Z'0332', KIND=CK)     !Combining Low Li
ne

    open(output_unit,encoding='utf-8')

    write(output_unit,*) s
    write(output_unit,*) 'len(s) = ', len(s)
    write(output_unit,*) 's(1:1) = ', s(1:1)

end program test


   I get the results
 a̐éö̲
 len(s) =            7
 s(1:1) = a

   [47]FortranFan February 11, 2022, 3:09pm 16

   plevold:

     selected_char_kind('ISO_10646') in Fortran would be similar to
     wchar_t in C++.

   I personally think the Fortran standard has taken the right step with
   ISO 10646 which is UCS that is rather close to Unicode but it is not
   strictly the same as Unicode, if I recall correctly. UCS is a better
   place in terms of character sets and it can be any of the popular
   encodings, UTF-8 if a Fortran processor so chooses.

   With respect to Fortran or any standards-based language for that matter
   with multiple possible processor implementations, the details will
   always be in the processor-dependent category. It’s up to the
   processors to converge to a good place, UTF-8 might just be it. It will
   be highly beneficial if IFORT steps up with character sets beyond its
   default.

   1 Like

   [48]wclodius February 12, 2022, 12:11pm 17

   UCS is just the standardization of the Code Points, and their names, of
   Unicode. In addition to specifying the code points and their names,
   Unicode also maintains a large data base of the properties of the code
   points, i.e., are they letters, symbols, numbers,…, if letters are they
   upper, lower, title case, or uncased, if they are numbers what are
   their values, if they represent composites of other code points what
   are the other code points, …

   [49]sblionel February 12, 2022, 6:15pm 18

   plevold:

   FortranFan:

     Any particular hurdle that prevents IFORT from supporting the wider
     character set?

     Only speculation from my side, but perhaps Intel has acknowledged
     the issue with wide character types and because of this don’t
     bothered implementing support for it?

   No - just perceived lack of demand, and resource constraints. gfortran
   developers (the few that there are) tend to chase the “shiny” things
   even when there are large, known gaps in support for the standard (I
   don’t think gfortran really does all of F2003 yet.) I expect Intel will
   get to alternate character sets eventually. It does have a nice library
   for dealing with multinational character sets.

   [50]plevold February 12, 2022, 7:52pm 19

   FortranFan:

     With respect to Fortran or any standards-based language for that
     matter with multiple possible processor implementations, the details
     will always be in the processor-dependent category. It’s up to the
     processors to converge to a good place, UTF-8 might just be it. It
     will be highly beneficial if IFORT steps up with character sets
     beyond its default.

   I don’t think I agree with you on this. As long the processor
   interprets the contents of a string literal as a sequence of bytes
   (which is indeed is) and don’t try to do any conversion then I think
   one can use whatever encoding one wish.

   Where one will be processor dependent is if one wishes to use escape
   sequences to insert code point (e.g. \u0041 instead of a), but this
   does not seem to be covered by the Fortran standard?

   Encoding matters at the application boundaries, e.g. when outputting
   text to a terminal or a file. Here, Unicode, and particularly UTF-8,
   seems to be very well supported.

   UTF-8 code points can be between 1 and 4 bytes long. As such they fit
   very well into the default character kind where one element is one
   byte. The notion that one element corresponds to one user perceived
   character or grapheme cluster is false when using Unicode, regardless
   the size of the element. This is why I prefer the default character
   kind over selected_char_kind('ISO_10646') which, at least when using
   gfortran, is 4 bytes long:
    integer, parameter :: ucs2 = selected_char_kind('ISO_10646')
    write(*,*) sizeof('a'), sizeof(ucs2_'a')

                    1                    4

   [51]apoorv01 February 8, 2024, 1:11pm 20

   [52]@drikosev
   I am getting this error when trying to compile the test code you have
   given for unicode
   “A BOZ literal constant at (1) cannot appear as an actual argument in a
   function reference”

   The version of GFORTRAN I am using is 13 on Ubuntu

   Regards
   Apurva

   The reference program is
program test
    use iso_fortran_env
    implicit none

    integer,parameter :: CK = selected_char_kind('ISO_10646')

    !see also https://en.wikipedia.org/wiki/Combining_Diacritical_Marks
    !see also https://www.compart.com/en/unicode/U+006F
    character(kind=CK,len=7) :: s = CHAR(Z'0061', KIND=CK) //& !a
                                  & CHAR(Z'0310', KIND=CK) //& !◌̐ -> a̐
                                ! & CHAR(Z'00E9', KIND=CK) //& !é (precomposed)
                                  & CHAR(Z'0065', KIND=CK) //& !e (decomposed)
                                  & CHAR(Z'0301', KIND=CK) //& !◌́
                                ! & CHAR(Z'00F6', KIND=CK) //& !ö  (precomposed
)
                                  & CHAR(Z'006F', KIND=CK) //& !o (decomposed)
                                  & CHAR(Z'0308', KIND=CK) //& !Combining Diaere
sis
                                  & CHAR(Z'0332', KIND=CK)     !Combining Low Li
ne

    open(output_unit,encoding='utf-8')

    write(output_unit,*) s
    write(output_unit,*) 'len(s) = ', len(s)
    write(output_unit,*) 's(1:1) = ', s(1:1)

end program test


   [53]next page →

Related topics

   Topic Replies Views Activity
   [54]Culture setting / inoculation against squiggles
   [55]Help

   35 1262 June 2, 2023
   [56]How do I file-read French special characters like é etc?

   [57]Help

   46 2377 January 22, 2024
   [58]Formatted hexadecimal output in lower case

   [59]Help

   25 428 September 24, 2024
   [60]Handling files in directories whose names are not entirely ASCII

   [61]Help

   8 1087 November 23, 2022
   [62]How do i allocate an array of strings?

   71 5468 September 30, 2023

     * [63]Home
     * [64]Categories
     * [65]Guidelines
     * [66]Terms of Service
     * [67]Privacy Policy

   Powered by [68]Discourse, best viewed with JavaScript enabled

References

   Visible links:
   1. https://fortran-lang.discourse.group/opensearch.xml
   2. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764?page=2
   3. https://fortran-lang.discourse.group/
   4. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764
   5. https://fortran-lang.discourse.group/c/tutorials/8
   6. https://fortran-lang.discourse.group/u/plevold
   7. https://github.com/plevold/unicode-in-fortran
   8. https://github.com/plevold/unicode-in-fortran/issues
   9. http://utf8everywhere.org/
  10. https://github.com/microsoft/terminal
  11. https://akr.am/blog/posts/using-utf-8-in-the-windows-terminal
  12. https://gitforwindows.org/
  13. https://marketplace.visualstudio.com/items?itemName=brunnerh.insert-unicode
  14. http://utf8everywhere.org/#characters
  15. https://hsivonen.fi/string-length/
  16. http://utf8everywhere.org/#textops
  17. https://en.wikipedia.org/wiki/Byte_order_mark
  18. https://fortran-lang.discourse.group/t/handling-files-in-directories-whose-names-are-not-entirely-ascii/4760/4
  19. https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949/2
  20. https://fortran-lang.discourse.group/u/Arjen
  21. https://fortran-lang.discourse.group/u/jacobwilliams
  22. https://github.com/jacobwilliams/json-fortran
  23. https://fortran-lang.discourse.group/u/FortranFan
  24. https://github.com/jacobwilliams/json-fortran
  25. https://fortran-lang.discourse.group/u/greenrongreen
  26. https://fortran-lang.discourse.group/u/plevold
  27. https://fortran-lang.discourse.group/u/jacobwilliams
  28. http://utf8everywhere.org/
  29. https://fortran-lang.discourse.group/u/drikosev
  30. http://gist.github.com/drikosev/d35956f266ff7af49074e7e669cd34df
  31. https://fortran-lang.discourse.group/u/plevold
  32. https://crates.io/crates/unicode-segmentation
  33. https://www.babelstone.co.uk/Unicode/whatisit.html
  34. https://fortran-lang.discourse.group/u/drikosev
  35. https://fortran-lang.discourse.group/u/plevold
  36. https://fortran-lang.discourse.group/u/vmagnin
  37. https://fortran-lang.discourse.group/u/drikosev
  38. https://fortran-lang.discourse.group/u/plevold
  39. https://crates.io/crates/unicode-segmentation
  40. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764/7
  41. https://fortran-lang.discourse.group/u/drikosev
  42. https://fortran-lang.discourse.group/u/plevold
  43. http://utf8everywhere.org/#characters
  44. https://developer.twitter.com/en/docs/basics/counting-characters.html
  45. https://fortran-lang.discourse.group/t/culture-setting-inoculation-against-squiggles/5857/23
  46. https://fortran-lang.discourse.group/u/drikosev
  47. https://fortran-lang.discourse.group/u/FortranFan
  48. https://fortran-lang.discourse.group/u/wclodius
  49. https://fortran-lang.discourse.group/u/sblionel
  50. https://fortran-lang.discourse.group/u/plevold
  51. https://fortran-lang.discourse.group/u/apoorv01
  52. https://fortran-lang.discourse.group/u/drikosev
  53. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764?page=2
  54. https://fortran-lang.discourse.group/t/culture-setting-inoculation-against-squiggles/5857
  55. https://fortran-lang.discourse.group/c/help/5
  56. https://fortran-lang.discourse.group/t/how-do-i-file-read-french-special-characters-like-e-etc/6618
  57. https://fortran-lang.discourse.group/c/help/5
  58. https://fortran-lang.discourse.group/t/formatted-hexadecimal-output-in-lower-case/8637
  59. https://fortran-lang.discourse.group/c/help/5
  60. https://fortran-lang.discourse.group/t/handling-files-in-directories-whose-names-are-not-entirely-ascii/4760
  61. https://fortran-lang.discourse.group/c/help/5
  62. https://fortran-lang.discourse.group/t/how-do-i-allocate-an-array-of-strings/3930
  63. https://fortran-lang.discourse.group/
  64. https://fortran-lang.discourse.group/categories
  65. https://fortran-lang.discourse.group/guidelines
  66. https://fortran-lang.discourse.group/tos
  67. https://fortran-lang.discourse.group/privacy
  68. https://www.discourse.org/

   Hidden links:
  70. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764#introduction-1
  71. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764#compilers-2
  72. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764#creating-and-printing-unicode-strings-3
  73. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764#determining-the-length-of-a-unicode-string-4
  74. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764#searching-for-substrings-5
  75. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764#reading-and-writing-to-file-6
  76. https://fortran-lang.discourse.group/t/using-unicode-characters-in-fortran/2764#conclusion-7
