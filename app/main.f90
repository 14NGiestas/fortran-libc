program test_libc
    use libc
    use, intrinsic :: iso_fortran_env
    implicit none

    call clock_example
    call mktime_example(2022,01,22) ! Saturday
    call difftime_example
    call localtime_example
    call time_example
    call strptime_example
    call strftime_example
    call asctime_example
    call gmtime_example
    call memory_example

contains

    function frequency_of_primes(n) result(freq)
        integer, intent(in) :: n
        integer :: i,j,freq
        real(REAL64) :: is
        freq = n - 1
        do i = 2, n
            is = sqrt(real(i,REAL64))
            do j = int(is), 2, -1
                if (mod(i,j) == 0) then
                    freq = freq - 1
                    exit
                end if
            end do
        end do
    end function

    subroutine clock_example
        integer(c_clock_t) :: clock_start, clock_end, clocks_per_sec
        real(REAL64) :: time_start, time_end
        integer :: freq

        print '("> clock example")'

        clock_start = clock()
        call cpu_time(time_start)

        write(*,*) "Calculating..."
        freq = frequency_of_primes(99999)
        write(*,*) "The number of primes lower than 100,000 is: ", freq

        clock_end = clock()
        call cpu_time(time_end)
        clocks_per_sec = get_clocks_per_sec()
        print *, "clocks_per_sec = ", clocks_per_sec
        write(*,'("It took me ", i0," clicks (",f0.5," seconds)")') &
            clock_end-clock_start, &
            real(clock_end - clock_start,REAL64)/real(clocks_per_sec,REAL64)
        write(*,'("Fortran version: ",f0.5," seconds")') time_end - time_start
    end subroutine

    subroutine mktime_example(year, month, day)
        integer(c_int), optional :: year, month, day
        integer(c_time_t) :: rawtime, thattime
        type(c_tm_t)      :: timeinfo
        integer(c_int)    :: Y, M, D
        character(len=9) :: &
            weekday(0:6) = [character(len=9) :: &
                "Sunday",    "Monday",   "Tuesday", &
                "Wednesday", "Thursday", "Friday", "Saturday"]

        write(output_unit,'(A)') "> mktime example"
        if (.not. present(year)) then
            write(output_unit,"(A)",advance='no') "Enter year: "
            read(input_unit,*) Y
        else
            Y = year
        end if
        if (.not. present(month)) then
            write(output_unit,"(A)",advance='no') "Enter month: "
            read(input_unit,*) M
        else
            M = month
        end if
        if (M > 12 .or. M < 1) error stop "Invalid month"

        if (.not. present(day)) then
            write(output_unit,"(A)",advance='no') "Enter day: "
            read(input_unit,*) D
        else
            D = day
        end if
        if (M > 31 .or. M < 1) error stop "Invalid day"

        rawtime = time()
        timeinfo = localtime(rawtime)
        timeinfo % tm_year = Y - 1900
        timeinfo % tm_mon  = M - 1
        timeinfo % tm_mday = D

        thattime = mktime(timeinfo)
        timeinfo = localtime(thattime)

        write(output_unit,'("The input date is a ",A)') weekday(timeinfo%tm_wday)
    end subroutine

    subroutine difftime_example
        integer(c_time_t) :: now
        type(c_tm_t)   :: newyear
        real(c_double) :: seconds

        print '("> difftime example")'

        now = time()
        newyear = localtime(now)
        ! sec, min, hour, mday, mon, year, wday, yday, isdst
        print*, newyear

        newyear % tm_hour = 0 ! hour
        newyear % tm_min  = 0
        newyear % tm_sec  = 0
        newyear % tm_mon  = 0
        newyear % tm_mday = 1
        print *, newyear

        seconds = difftime(now, mktime(newyear))
        print '(i0," seconds since ",A," in the current timezone")',&
            int(seconds), "New Year"
        print '(i0," days since ",A," in the current timezone")',   &
            int(seconds/(24.*3600.)), "New Year"
    end subroutine

    subroutine localtime_example
        integer(c_time_t) :: rawtime
        type(c_tm_t) :: timeinfo

        print '("> localtime example")'

        rawtime = time()
        timeinfo = localtime(rawtime)

        write(*,*) "Current local time and date: ", asctime(timeinfo)
    end subroutine

    subroutine time_example
        integer(c_time_t) :: timer, that_time
        type(c_tm_t) :: y2k
        real(c_double) :: seconds

        print '("> time example")'

        y2k = c_tm_t()
        y2k % tm_mday = 1
        y2k % tm_year = 100
        y2k % tm_mon  = 0

        print *, y2k

        timer = time(timer)

        that_time = mktime(y2k)
        seconds = difftime(timer,that_time)
        print '(i0," seconds since ",A," in the current timezone")',&
            int(seconds), "January 1, 2000"
        print '(i0," days since ",A," in the current timezone")',   &
            int(seconds/(24.*3600.)), "January 1, 2000"

    end subroutine

    subroutine strptime_example
        integer(c_time_t) :: rawtime
        type(c_tm_t)      :: timeinfo
        character(:), allocatable :: ret
        real(c_double) :: seconds

        print '("> strptime example")'

        rawtime = time(rawtime)

        timeinfo = c_tm_t()

        ret = strptime("1992-08-21", "%F", timeinfo)
        if (ret /= c_null_char) then
            seconds = difftime(rawtime, mktime(timeinfo))
            write(*,*) seconds, " seconds since I was born"
            write(*,*) seconds/(24.*3600.), " days since I was born"
            write(*,*) seconds/(365.24*24.*3600.), " years since I was born"
        else
            error stop
        end if
    end subroutine

    subroutine strftime_example
        integer(c_time_t) :: rawtime
        type(c_tm_t) :: timeinfo

        character(len=80) :: buffer
        integer(c_size_t) :: ret

        print '("> strftime_example")'

        rawtime = time(rawtime)
        timeinfo = localtime(rawtime)
        ret = f_strftime(buffer, 80, "Now it's %I:%M %p.", timeinfo)
        write(*,*) buffer(1:ret)
    end subroutine

    subroutine asctime_example
        integer(c_time_t) :: rawtime
        type(c_tm_t)      :: timeinfo

        print '("> asctime example")'

        rawtime = time(rawtime)
        timeinfo = localtime(rawtime)

        print '("The current date/time is: ",A)', asctime(timeinfo)
    end subroutine

    subroutine gmtime_example
        integer(c_time_t) :: rawtime
        type(c_tm_t) :: tm
        integer(c_int), parameter :: MST = -7, UTC = 0, CCT = 8
        integer(c_int) :: hour, minutes

        print '("> gmtime example")'

        rawtime = time()
        tm = gmtime(rawtime)
        hour    = tm % tm_hour
        minutes = tm % tm_min

        print '("Current time around the world:")'
        print '("  Phoenix, AZ (U.S.):  ",i0.2,":",i0.2)', abs(mod(hour+MST,24)), minutes
        print '("  Reykjavik (Iceland): ",i0.2,":",i0.2)', abs(mod(hour+UTC,24)), minutes
        print '("  Beijing (China):     ",i0.2,":",i0.2)', abs(mod(hour+CCT,24)), minutes
    end subroutine

    subroutine memory_example
        integer, parameter :: MAX_SIZE = 5
        real(c_double)          :: a
        real(c_double), pointer :: b(:) => null()
        type(c_ptr)             :: b_ptr

        print '("> memory example")'

        b_ptr = c_malloc(c_sizeof(a)*MAX_SIZE)
        call c_f_pointer(b_ptr, b, [MAX_SIZE])
        if (.not. size(b) == MAX_SIZE) error stop "Assertion failed"

        b = 1.0
        b_ptr = c_realloc(b_ptr, c_sizeof(a)*(2*MAX_SIZE))
        call c_f_pointer(b_ptr, b, [2*MAX_SIZE])

        if (.not. size(b) == 2*MAX_SIZE) error stop "Assertion failed"

        b(MAX_SIZE+1:) = 2

        if (.not. all(abs(b(1:MAX_SIZE) - 1)  < epsilon(a))) error stop "Assertion failed"
        if (.not. all(abs(b(MAX_SIZE+1:) - 2) < epsilon(a))) error stop "Assertion failed"

        if (.not. c_associated(b_ptr)) error stop "Assertion failed"
        if (.not.   associated(b))     error stop "Assertion failed"

        call c_free(b_ptr)
        b_ptr = c_null_ptr
        nullify(b)

        if (c_associated(b_ptr)) error stop "Assertion failed"
        if (  associated(b))     error stop "Assertion failed"

        call c_f_pointer(b_ptr, b, [MAX_SIZE])

        if (c_associated(b_ptr)) error stop "Assertion failed"
        if (  associated(b))     error stop "Assertion failed"
    end subroutine

end program
