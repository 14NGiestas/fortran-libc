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
end module
