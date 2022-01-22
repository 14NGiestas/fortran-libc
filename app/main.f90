program test_libc
    use libc
    use, intrinsic :: iso_fortran_env
    implicit none

    call clock_example
    call difftime_example
    call time_example
    call asctime_example
    call localtime_example
    call memory_example
    call strptime_example

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
        integer :: i, freq

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

    subroutine difftime_example
        integer(c_time_t) :: now, new_year
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
        seconds = difftime(rawtime, mktime(timeinfo))
        write(*,*) seconds, " seconds since I was born"
        write(*,*) seconds/(24.*3600.), " days since I was born"
        write(*,*) seconds/(365.24*24.*3600.), " years since I was born"
    end subroutine

    subroutine asctime_example
        integer(c_time_t) :: rawtime
        type(c_tm_t)      :: timeinfo
        character(:), allocatable :: ret

        print '("> asctime example")'

        rawtime = time(rawtime)
        timeinfo = localtime(rawtime)

        ret = asctime(timeinfo)
        print '("The current date/time is: ",A)', ret
    end subroutine

    subroutine memory_example
        integer, parameter :: MAX_SIZE = 5
        real(c_double)          :: a
        real(c_double), pointer :: b(:) => null()
        type(c_ptr)             :: b_ptr

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
