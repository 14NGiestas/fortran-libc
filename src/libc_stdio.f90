module libc_stdio
    use, intrinsic :: iso_c_binding
    implicit none
    !
    ! <stdio.h>
    !
    interface
        subroutine c_perror(str) bind(c,name='perror')
            import c_char
            character(kind=c_char) :: str(*)
        end subroutine
    end interface

    interface perror
        procedure :: c_perror
        procedure :: f_perror
    end interface

contains

    subroutine f_perror(str)
        character(*) :: str
        call c_perror(str // c_null_char)
    end subroutine

end module
