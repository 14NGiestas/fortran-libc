module libc_stdlib
    use, intrinsic :: iso_c_binding
    implicit none
    !
    ! <stdlib.h>
    !
    interface
        !-----------------------------------
        ! Pseudo-random sequence generation
        !-----------------------------------

        ! int rand (void);
        integer(c_int) &
        function c_rand() bind(c,name='rand')
            !! Generate random number
            import c_int
        end function

        ! void srand (unsigned int seed);
        subroutine c_srand(seed) bind(c,name='srand')
            !! Initialize random number generator
            import c_int
            integer(c_int), value :: seed
                !! An integer value to be used as seed by the pseudo-random number generator algorithm.
        end subroutine

        ! custom: see libc.c
        integer(c_int) function c_get_rand_max() bind(c,name='get_rand_max')
            !! Maximum value returned by rand
            import c_int
        end function


        !---------------------------
        ! Dynamic memory management
        !---------------------------

        ! void* malloc (size_t size);
        type(c_ptr) &
        function c_malloc(size) bind(c,name='malloc')
            !! Allocate memory block
            import c_ptr, c_size_t
            integer(c_size_t), intent(in), value :: size
        end function

        ! void free (void* ptr);
        subroutine c_free(ptr) bind(c,name='free')
            !! Deallocate memory block
            import c_ptr
            type(c_ptr), value :: ptr
        end subroutine

        ! void* realloc (void* ptr, size_t size);
        type(c_ptr) &
        function c_realloc(ptr,new_size) bind(c,name='realloc')
            !! Reallocate memory block
            import c_ptr, c_size_t
            type(c_ptr), value :: ptr
            integer(c_size_t), intent(in), value :: new_size
        end function

        ! void* calloc (size_t num, size_t size);
        type(c_ptr) &
        function c_calloc(num,size) bind(c,name='calloc')
            !! Allocate and zero-initialize array
            import c_ptr, c_size_t
            integer(c_size_t), intent(in), value :: num !! number of objects
            integer(c_size_t), intent(in), value :: size !! size of each object (in bytes)
        end function

        !-----------------------
        ! Searching and sorting
        !-----------------------

        ! void qsort (void* base, size_t num, size_t size,
        !             int (*compar)(const void*,const void*));
        subroutine c_qsort(base, num, size, compar) bind(c,name='qsort')
            !! Sort elements of array
            import c_ptr, c_size_t, c_funptr
            type(c_ptr), value :: base
            integer(c_size_t), value :: num, size
            type(c_funptr), value :: compar
        end subroutine
    end interface
end module
