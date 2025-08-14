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
