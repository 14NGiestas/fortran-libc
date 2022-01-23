module libc
    use, intrinsic :: iso_c_binding
    use libc_time
    use libc_errno
    use libc_stdio
    use libc_stdlib
    use libc_string
    implicit none

contains


!     function cmktime(timeptr) result(res)
!         type(c_tm_t), intent(in) :: timeptr
!         type(c_time_t) :: res
!         res = mktime(timeptr)
!     end function

!     function cctime(timer) result(res)
!         type(c_time_t), optional :: timer
!         type(c_time_t) :: res
!         if (present(timer)) then
!             res = time(timer)
!         else
!             res = time(c_null_ptr)
!         end if
!     end function

!     subroutine set(tm,hour,min,sec,year,mon,mday)
!         type(c_tm_t), intent(inout) :: tm
!         integer(c_int), intent(in) :: hour,min,sec,year,mon,mday
!         interface
!             subroutine set_tm(ptr,hour,min,sec,year,mon,mday) bind(c,name='set_tm')
!                 import c_ptr, c_int
!                 type(c_ptr), value :: ptr
!                 integer(c_int), intent(in) :: hour,min,sec,year,mon,mday
!             end subroutine
!         end interface
!         !tm = cmalloc(c_sizeof(hour)*9)
!         call set_tm(tm,hour,min,sec,year,mon,mday)
!     end subroutine

!     ! subroutine destroy_ctm(this)
!     !     type(c_tm_t) :: this

!     function cdifftime(end,beginning) result(res)
!         type(c_time_t), intent(in) :: end, beginning
!         real(c_double) :: res
!         res = difftime(end,beginning)
!     end function

!     function casctime(tm) result(res)
!         type(c_tm_t), intent(in) :: tm
!         character(len=25), pointer :: f_string
!         character(len=24) :: res
!         type(c_ptr) :: c_string

!         c_string = asctime(tm)
!         call c_f_pointer(c_string,f_string)
!         res = f_string(1:24)
!     end function

! #if 0
!     function cclocaltime(time) result(res)
!         type(c_time_t) :: time
!         type(c_tm_t) :: res
!         res = localtime(c_loc(time))
!         ! call c_f_pointer(res,res%raw,[9])
!     end function

! #endif
end module

