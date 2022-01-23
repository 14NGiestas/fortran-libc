module libc_time
    use, intrinsic :: iso_c_binding
    use libc_string
    implicit none
    !
    ! <time.h>
    !
    type, bind(c) :: c_tm_t
        integer(c_int) :: tm_sec
        integer(c_int) :: tm_min
        integer(c_int) :: tm_hour
        integer(c_int) :: tm_mday
        integer(c_int) :: tm_mon
        integer(c_int) :: tm_year
        integer(c_int) :: tm_wday
        integer(c_int) :: tm_yday
        integer(c_int) :: tm_isdst
    end type

    interface c_tm_t
        procedure :: c_tm_t_init_null
        procedure :: c_tm_t_init_array
    end interface

    ! https://stackoverflow.com/questions/3465517/what-is-size-of-the-time-variable-in-c-programming#3465546
    integer, parameter :: c_time_t  = c_long !! clock_t is just a macro/wrapper to a c_long
    integer, parameter :: c_clock_t = c_long !! time_t is just a macro/wrapper to a c_long

    interface

        ! clock_t clock (void);
        function c_clock() result(res) bind(c,name='clock')
            !! Clock program
            import c_ptr, c_clock_t
            integer(c_clock_t) :: res
        end function

        ! custom: see libc.c
        function get_clocks_per_sec() result(res) bind(c,name='get_clocks_per_sec')
            !! Clock ticks per second
            !! This macro evaluates to an expression of type clock_t.
            import c_ptr, c_clock_t
            integer(c_clock_t) :: res
        end function


        ! double difftime (time_t end, time_t beginning);
        function c_difftime(end, beginning) result(res) bind(c,name='difftime')
            !! Return difference between two times
            import c_double, c_time_t
            integer(c_time_t), value :: end
                !! Higher bound of the time interval whose length is calculated.
            integer(c_time_t), value :: beginning
                !! Lower bound of the time interval whose length is calculated.
                !! If this describes a time point later than end, the result is negative.
            real(c_double) :: res
                !! The result of (end-beginning) in seconds as a floating-point value of type double.
        end function

        ! time_t mktime (struct tm * timeptr);
        integer(c_time_t) &
        function c_mktime(timeptr) bind(c,name='mktime')
            !! Convert tm structure to time_t
            import c_ptr, c_time_t
            type(c_ptr), value :: timeptr
                !! Pointer to a tm structure that contains a calendar time broken down into its components
        end function

        ! time_t time (time_t* timer);
        type(c_ptr) function c_time(timer) bind(c,name='time')
            !! Get current time
            import c_ptr
            type(c_ptr), value :: timer
        end function

        integer(c_time_t) function f_time(timer) bind(c,name='time')
            import c_time_t
            integer(c_time_t), optional :: timer
        end function

        ! char* asctime (const struct tm * timeptr);
        function c_asctime(timeptr) result(carr) bind(c,name='asctime')
            !! Convert tm structure to string
            import c_ptr
            type(c_ptr), value :: timeptr
            type(c_ptr) :: carr
        end function

        ! struct tm * gmtime (const time_t * timer);
        type(c_ptr) &
        function c_gmtime(timer) bind(c,name='gmtime')
            !! Convert time_t to tm as UTC time
            import c_ptr
            type(c_ptr), intent(in), value :: timer
        end function

        ! struct tm * localtime (const time_t * timer)
        type(c_ptr) &
        function c_localtime(timer) bind(c,name='localtime')
            !! Convert time_t to tm as local time
            import c_ptr
            type(c_ptr), intent(in), value :: timer
        end function

        ! size_t strftime (char* ptr, size_t maxsize, const char* format,
        !                  const struct tm* timeptr );
        function c_strftime(ptr,maxsize,format,timeptr) result(res) bind(c,name='strftime')
            !! Format time as string
            import c_ptr, c_size_t, c_char
            character(kind=c_char), intent(out) :: ptr(*)
                !! Pointer to the destination array where the resulting C string is copied.
            integer(c_size_t), value :: maxsize
                !! Maximum number of characters to be copied to ptr, including the terminating null-character.
            character(kind=c_char), intent(in) :: format(*)
                !! C string containg any combination of regular characters and special format specifiers.
            type(c_ptr), intent(in), value :: timeptr
                !! Pointer to a tm structure that contains a calendar time broken down into its components.
            integer(c_size_t) :: res
        end function

        ! char *strptime(const char *buf, const char *format, struct tm *tm);
        function c_strptime(buffer, format, tm) result(res) bind(c,name='strptime')
            !! The strptime() function converts the character string pointed to by buf to values which are stored
            !! in the tm structure pointed to by tm, using the format specified by format.
            import c_ptr, c_char
            character(kind=c_char), intent(in) :: buffer(*)
            character(kind=c_char), intent(in) :: format(*)
            type(c_ptr), value :: tm
            type(c_ptr) :: res
        end function
    end interface

    interface clock
        procedure :: c_clock
    end interface

    interface time
        procedure :: c_time
        procedure :: f_time
    end interface

    interface mktime
        procedure :: c_mktime
        procedure :: f_mktime
    end interface

    interface difftime
        procedure :: c_difftime
    end interface

    interface gmtime
        procedure :: c_gmtime
        procedure :: f_gmtime
    end interface

    interface localtime
        procedure :: c_localtime
        procedure :: f_localtime
    end interface

    interface asctime
        procedure :: c_asctime
        procedure :: f_asctime
    end interface

    interface strptime
        procedure :: c_strptime
        procedure :: f_strptime
    end interface

    interface strftime
        procedure :: c_strftime
        procedure :: f_strftime
    end interface

contains

    ! size_t strftime (char* ptr, size_t maxsize, const char* format,
    !                  const struct tm* timeptr );
    function f_strftime(ptr, maxsize, format, timeptr) result(res)
        !! Format time as string
        character(*),      intent(out) :: ptr
        integer,           intent(in)  :: maxsize
        character(*),      intent(in)  :: format
        type(c_tm_t), target :: timeptr
        integer(c_size_t) :: res
        res = c_strftime(ptr, int(maxsize,c_size_t), format//c_null_char, c_loc(timeptr))
    end function

    ! struct tm * gmtime (const time_t * timer);
    function f_gmtime(timer) result(res)
        integer(c_time_t), target :: timer
        type(c_tm_t), pointer :: f_ptr
        type(c_tm_t) :: res
        type(c_ptr) :: res_ptr
        res_ptr = c_gmtime(c_loc(timer))
        call c_f_pointer(res_ptr, f_ptr)
        res = f_ptr
    end function

    ! time_t mktime (struct tm * timeptr);
    function f_mktime(timeptr)
        !! Convert tm structure to time_t
        type(c_tm_t), target :: timeptr
        integer(c_time_t) :: f_mktime
        f_mktime = c_mktime(c_loc(timeptr))
    end function

    ! struct tm * localtime (const time_t * timer)
    function f_localtime(timer) result(res)
        !! Convert time_t to tm as local time
        integer(c_time_t), intent(in), target :: timer
        type(c_tm_t), pointer :: f_ptr
        type(c_tm_t)          :: res
        type(c_ptr)           :: res_ptr
        res_ptr = c_localtime(c_loc(timer))
        call c_f_pointer(res_ptr, f_ptr)
        res = f_ptr
    end function

    ! char* asctime (const struct tm * timeptr);
    function f_asctime(timeptr) result(res)
        type(c_tm_t), target :: timeptr
        type(c_ptr) :: c_string
        character(:), allocatable :: res

        c_string = c_asctime(c_loc(timeptr))
        res = c_f_string(c_string)
    end function

    ! char *strptime(const char *buf, const char *format, struct tm *tm);
    function f_strptime(buffer, format, tm) result(res)
        character(*), intent(in) :: buffer
        character(*), intent(in) :: format
        type(c_tm_t), target :: tm
        type(c_ptr) :: c_string
        character(:), allocatable :: res

        c_string = c_strptime(buffer//c_null_char, format//c_null_char, c_loc(tm))
        res = c_f_string(c_string)
    end function

    function c_tm_t_init_null() result(new)
        type(c_tm_t) :: new
        new % tm_sec  = 0_c_int
        new % tm_min  = 0_c_int
        new % tm_hour = 0_c_int
        new % tm_mday = 0_c_int
        new % tm_mon  = 0_c_int
        new % tm_year = 0_c_int
        new % tm_wday = 0_c_int
        new % tm_yday = 0_c_int
        new % tm_isdst = 0_c_int
    end function

    function c_tm_t_init_array(array) result(new)
        type(c_tm_t) :: new
        integer(c_int), intent(in) :: array(:)
        new % tm_sec   = array(1)
        new % tm_min   = array(2)
        new % tm_hour  = array(3)
        new % tm_mday  = array(4)
        new % tm_mon   = array(5)
        new % tm_year  = array(6)
        new % tm_wday  = array(7)
        new % tm_yday  = array(8)
        new % tm_isdst = array(9)
    end function

end module
