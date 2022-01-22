module libc_string
    use, intrinsic :: iso_c_binding
    implicit none
    !
    ! <string.h>
    !
    interface
        ! size_t strlen ( const char * str );
        function c_strlen(ptr) result(res) bind(c,name='strlen')
            !! Get string length
            import c_size_t, c_ptr
            type(c_ptr), value :: ptr !! C string.
            integer(c_size_t) :: res !! The length of string.
        end function

        type(c_ptr) &
        function strerror(errnum) bind(c,name='strerror')
            import c_ptr, c_int
            integer(c_int), intent(in), value :: errnum
        end function

    end interface

    interface c_f_string
        module procedure c_f_string_array
        module procedure c_f_string_ptr
    end interface

contains

    pure function c_f_string_array(c_string) result(f_string)
        character(c_char), intent(in)  :: c_string(:)
        character(len=size(c_string)) :: f_string
        integer(kind=c_size_t)  :: i
        do i=1, size(c_string)
            f_string(i:i) = c_string(i)
        end do
    end function

    function c_f_string_ptr(c_string) result(f_string)
        type(c_ptr), intent(in) :: c_string
        character(c_char), pointer :: f_array(:)
        character(:), allocatable :: f_string
        integer(kind=c_size_t) :: c_length
        f_string = ''
        c_length = c_strlen(c_string)
        if (.not. c_associated(c_string)) return
        if (.not. c_length > 0) return
        call c_f_pointer(c_string, f_array, [c_length])
        f_string = c_f_string_array(f_array)
    end function



end module
