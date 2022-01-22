module libc_errno
    use, intrinsic :: iso_c_binding
    implicit none
    !
    ! <errno.h>
    !
    interface
        ! custom: see libc.c
        subroutine set_errno(n) bind(c,name='set_errno')
            import c_int
            integer(c_int), intent(in), value :: n
        end subroutine

        ! custom: see libc.c
        integer(c_int) &
        function get_errno() bind(c,name='get_errno')
            import c_int
        end function

        ! custom: see libc.c
        integer(c_int) &
        function test_strerror() bind(c,name='test_strerror')
            import c_int
        end function
    end interface
end module
