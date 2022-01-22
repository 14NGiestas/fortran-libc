program test_libc

    use libc
    use iso_c_binding

    implicit none

    !
    ! <time.h>
    !
    ! call mktime_example ! Expects user input
    call gmtime_example
    call localtime_example
    call strftime_example

    !
    ! <errno.h>
    !
    call strerror_example
    !
    ! <stdlib.h>
    ! call rand_example
    call srand_example
    call qsort_example

contains


    subroutine mktime_example
        use iso_fortran_env, only: output_unit, input_unit
        type(c_time_t), target :: rawtime, thattime
        type(c_tm_t),   target :: timeinfo
        integer(c_int) :: year, month, day
        integer(c_int), pointer :: itm(:)
        character(len=9) :: weekday(0:6) = [character(len=9) :: "Sunday","Monday","Tuesday",&
            "Wednesday","Thursday","Friday","Saturday"]

        write(output_unit,*) "mktime example"

        write(output_unit,"(A)",advance='no') "Enter year: "
        read(input_unit,*) year
        write(output_unit,"(A)",advance='no') "Enter month: "
        read(input_unit,*) month
        write(output_unit,"(A)",advance='no') "Enter day: "
        read(input_unit,*) day

        rawtime = time(rawtime)
        timeinfo = localtime(c_loc(rawtime))
        !call c_f_pointer(timeinfo,itm,[9])
        itm(6) = year - 1900
        itm(5) = month - 1
        itm(4) = day

        thattime = mktime(timeinfo)

        write(output_unit,"(A,A)") "That day is a ", weekday(itm(7))


        rawtime = time(rawtime)
        timeinfo = localtime(rawtime)
        !call c_f_pointer(timeinfo,itm,[9])
        itm(6) = year - 1900
        itm(5) = month - 1
        itm(4) = day

        thattime = mktime(timeinfo)
        write(output_unit,"(A,A)") "That day is a ", weekday(itm(7))

    end subroutine



    subroutine gmtime_example
        type(c_time_t), target :: rawtime
        type(c_time_t) :: frawtime
        type(c_tm_t) :: ptm, fptm
        integer(c_int), pointer :: itm(:)
        integer(c_int), parameter :: mst = -7, utc = 0, cct = 8

        write(*,*) new_line('a')//"gmtime_example"

        rawtime = time(rawtime)
        frawtime = time(frawtime)

        ptm = gmtime(c_loc(rawtime))
        fptm = gmtime(frawtime)
        print*, ptm

        !call c_f_pointer(ptm,itm,[11])
        write(*,*) "Current time around the world:"
        write(*,"(A,I2,A1,I2)") "Phoenix, AZ (U.S.) :  ", mod(itm(3)+MST,24), ":", itm(2)
        write(*,"(A,I2,A1,I2)") "Reykjavik (Iceland) : ", mod(itm(3)+UTC,24), ":", itm(2)
        write(*,"(A,I2,A1,I2)") "Beijing (China) :     ", mod(itm(3)+CCT,24), ":", itm(2)

        !call c_f_pointer(fptm,itm,[11])
        write(*,*) "Current time around the world:"
        write(*,"(A,I2,A1,I2)") "Phoenix, AZ (U.S.) :  ", mod(itm(3)+MST,24), ":", itm(2)
        write(*,"(A,I2,A1,I2)") "Reykjavik (Iceland) : ", mod(itm(3)+UTC,24), ":", itm(2)
        write(*,"(A,I2,A1,I2)") "Beijing (China) :     ", mod(itm(3)+CCT,24), ":", itm(2)
    end subroutine


    subroutine strftime_example
        type(c_time_t), target :: rawtime
        type(c_tm_t), target :: timeinfo

        character(len=80) :: buffer
        integer(c_size_t) :: ret

        write(*,*)  new_line('a')//"strftime_example"

        rawtime = time(rawtime)
        timeinfo = localtime(c_loc(rawtime))
        ! timeinfo = localtime(rawtime)
        ret = c_strftime(buffer,80_c_size_t,"Now it's %I:%M%p."//c_null_char, c_loc(timeinfo))
        write(*,*) buffer(1:ret)

        rawtime = time()
        timeinfo = localtime(rawtime)
        ret = c_strftime(buffer,80_c_size_t,"Now it's %I:%M%p."//c_null_char, c_loc(timeinfo))
        write(*,*) buffer(1:ret)
    end subroutine


    subroutine strerror_example
        integer(c_int) :: i

        write(*,*) new_line('a')//"strerror_example"
        i = test_strerror()
        write(*,'(A)') "The following error occurred: "//fstrerror(get_errno())
        call perror("The following error occurred"//c_null_char)
    end subroutine


    subroutine rand_example
        integer(c_int) :: isecret, iguess
        integer(c_int) :: count

        call system_clock(count)
        call c_srand(count)

        isecret = mod(c_rand(),10) + 1

        do
            write(*,'(A)',advance='no') "Guess the number (1 to 10): "
            read(*,*) iguess
            if (isecret < iguess) then
                write(*,*) "The secret number is lower"
            else if (isecret > iguess) then
                write(*,*) "The secret number is higher"
            end if

            if (isecret == iguess) exit
        end do

        write(*,*) "Congratulations!"
    end subroutine

    subroutine srand_example
        integer(c_int) :: count

        call c_srand(1)
        write(*,*) "First number: ", mod(c_rand(),100)
        call system_clock(count)
        call c_srand(count)
        write(*,*) "Random number: ", mod(c_rand(),100)
        call c_srand(1)
        write(*,*) "Again the first number: ", mod(c_rand(),100)
    end subroutine

    ! int compar (const void* p1, const void* p2);
    ! integer(c_int) function compare(a,b) bind(c)
    !     type(c_ptr), intent(in), value :: a, b
    !     integer(c_int), pointer :: a_, b_

    !     call c_f_pointer(a,a_)
    !     call c_f_pointer(b,b_)
    !     print *, a_, b_
    !     compare = a_ - b_
    ! end function

    integer(c_int) function compare(a,b) bind(c)
        integer(c_int), intent(in) :: a, b
        compare = a - b
        ! if (a < b) then
        !     compare = -1
        ! else if (a == b) then
        !     compare = 0
        ! else if (a > b) then
        !     compare = 1
        ! end if
    end function

    ! see also https://stackoverflow.com/questions/20941575/sorting-in-fortran-undefined-reference-to-qsort
    subroutine qsort_example

        ! integer(c_int), pointer :: values(:)
        integer(c_int), target :: values(6)
        type(c_ptr) :: ptr

        write(*,*) new_line('a')//"qsort_example"

        ! ptr = cmalloc(6*c_sizeof(values(1)))

        ! call c_f_pointer(ptr,values,[6])
        values = [40, 10, 100, 90, 20, 25]

        write(*,"(6(I0,:,X))") values
        ! call qsort(ptr,6_c_size_t,c_sizeof(values(1)),c_funloc(compare))
        call c_qsort(c_loc(values(1)),6_c_size_t,c_sizeof(values(1)),c_funloc(compare))
        write(*,"(6(I0,:,X))") values
    end subroutine
end program

