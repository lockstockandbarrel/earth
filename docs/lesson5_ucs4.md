## Introduction to Fortran Unicode support
### Lesson VI: reading UTF-8 strings from command lines

If your OS supports utf-8 as the default encoding it is likely you will
at some point want to pass a parameter from the command line that contains
multi-byte Unicode characters to your program.

But the GET_COMMAND_ARGUMENT() intrinsic specifies that the VALUE argument
is a "scalar character variable of default kind".

Perhaps you are going to use the value as a stream of bytes representing
utf-8 characters. In that case you may be fine, and be able to use the
string without converting it to the suported UCS-4 internal representation
used by Fortran.

But if that is not the case, Fortran does not provide a procedure for
converting utf-8 to ucs-4 unicode.

Because of that lack a collection of procedures that includes
such conversions is being assembled into Fortran modules at
[github.com/lockstockandbarrel/earth](github.com/lockstockandbarrel/earth).

But Fortran does the conversion needed when reading utf-8-encoded files
into ucs-4 variables. So far the following method has worked on every
system type I have had access to ...

```fortran
program read_commandline
! @(#) take command line argument utf-8 text and generate Fortran statement that represents the string
use, intrinsic :: iso_fortran_env, only : output_unit
implicit none
integer, parameter                     :: ucs4 = selected_char_kind ('ISO_10646')
character(len=*),parameter             :: form= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter             :: g= '(*(g0))'
integer                                :: i
character(len=:),allocatable           :: aline
character(len=:),allocatable           :: command_line
character(len=:,kind=ucs4),allocatable :: ustr
   command_line=getargs()          ! get string containing all command arguments as CHARACTER bytes
   ustr=utf8_to_ucs4(command_line) ! convert bytes to internal Fortran Unicode representation

   ! write the command line out as a Fortran variable expression using the CHAR() function
   open (output_unit, encoding='UTF-8')
   write(*,g) '! ENCODING:',command_line
   write(*,g) 'character(len=*,kind=ucs4),parameter :: variable= &'
   write(*,form)(ustr(i:i),i=1,len(ustr))
contains

function getargs() result(command_line)
integer                                :: length
character(len=:),allocatable           :: command_line
   call get_command(length=length)                 ! get command line length
   allocate(character(len=length) :: command_line) ! allocate string big enough to hold command line
   call get_command(command=command_line)          ! get command line as a string
   call get_command_argument(0,length=length)      ! remove argument 0
   command_line=adjustl(command_line(length+1:))
end function getargs

function utf8_to_ucs4(string) result(corrected)
character(len=*),intent(in)            :: string
character(len=:,kind=ucs4),allocatable :: corrected
character(len=(len(string)),kind=ucs4) :: line
integer                                :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')string
   rewind(lun)
   read(lun,'(A)')line
   close(lun)
   corrected=trim(line)
end function utf8_to_ucs4

function ucs4_to_utf8(ucs4_string) result(corrected)
character(len=*,kind=ucs4),intent(in) :: ucs4_string
character(len=:),allocatable          :: corrected
character(len=(len(ucs4_string)*4))   :: line
integer                               :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')ucs4_string
   rewind(lun)
   open(unit=lun,encoding='default')
   read(lun,'(A)')line
   close(lun)
   corrected=trim(line)
end function ucs4_to_utf8
   
end program read_commandline
```
An example run using the famous Confuscion expression
"己所不欲，勿施於人" (jǐ suǒ bù yù, wù shī yú rén) or
"What you do not want done to yourself, do not do to others":

```bash
read_commandline "己所不欲，勿施於人" 
```
```text
! ENCODING:己所不欲，勿施於人 
character(len=*,kind=ucs4),parameter :: variable= &
char(int(z'5DF1'),kind=ucs4)// &
char(int(z'6240'),kind=ucs4)// &
char(int(z'4E0D'),kind=ucs4)// &
char(int(z'6B32'),kind=ucs4)// &
char(int(z'FF0C'),kind=ucs4)// &
char(int(z'52FF'),kind=ucs4)// &
char(int(z'65BD'),kind=ucs4)// &
char(int(z'65BC'),kind=ucs4)// &
char(int(z'4EBA'),kind=ucs4)
```
## Summary

Command line arguments are returned as ASCII bytes by the
GET_COMMAND_ARGUMENT() and GET_COMMAND() procedures. But using Fortran's
ability to encode UTF-8 as UCS-4 when reading and writing external files
it is easy to create functions for converting between the two encodings,
to make up for the lack of any equivalent intrinsics.

Note that modules of related functions can be found at 
[github.com/lockstockandbarrel/earth](github.com/lockstockandbarrel/earth)

