program test_libc

    use libc
    use iso_c_binding

    implicit none

    call strerror_example
    call rand_example   ! Expects user input
    call srand_example
    call qsort_example

contains


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

