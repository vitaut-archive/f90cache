module mod_mfdebug

   ! Part of MUESLI Numerical Library
   ! Copyright É. Canot 2003-2011 -- IRISA/CNRS

!=======================================================================
!    utilitaries for user-debugging
!    (traces for error/warning)
!=======================================================================

#if defined _INTEL_IFC
   use ifcore, only : tracebackqq ! (muesli_trace)
   use ifport, only : beepqq ! (muesli_trace)
#endif

   implicit none

#ifndef _DEVLP
   private
#endif

!-----------------------------------------------------------------------
!                           global variables
!-----------------------------------------------------------------------

   ! not parameters: these variable can be redefined by the user
   ! (via msSetStdIO)
   integer :: STDERR = 0,                                               &
              STDIN  = 5,                                               &
              STDOUT = 6

   integer :: mf_message_level = 2 ! cf. 'msSetMsgLevel()'
   integer :: mf_traceback_level = 1 ! cf. 'msSetTrbLevel()'

   logical :: mf_debug_user_bool = .false.

   integer :: mf_fp_rounding_mode = 1 ! cf. 'msSetRoundingMode()'

!-----------------------------------------------------------------------
!                             parameters
!-----------------------------------------------------------------------

   integer, parameter :: MF_DOUBLE = kind(1.0d0)

   ! MF_ADDRESS is a kind for integers
#ifdef _64_BITS
   ! max int in 64 bits is ~ 1.8446744074E+019
   integer, parameter :: MF_ADDRESS = selected_int_kind(18) ! 64 bits
#else
   ! max int in 32 bits is ~ 2.1474836480E+009
   integer, parameter :: MF_ADDRESS = selected_int_kind(8)  ! 32 bits
#endif

   public :: STDERR, STDIN, STDOUT, MF_DOUBLE

   ! kind for one-byte integer
   integer, parameter :: kind_1 = selected_int_kind(r=2)

   integer(kind=kind_1), parameter :: UNKNOWN = -1,                     &
                                      FALSE   =  0,                     &
                                      TRUE    =  1

contains
!_______________________________________________________________________
!
   subroutine muesli_trace( pause )

      character(len=*), intent(in) :: pause
      !------ API end ------

#if defined _GNU_GFC
#ifndef _RELEASE
#ifdef _HAVE_SHOW_BACKTRACE
      external :: show_backtrace
#endif
#endif
#endif

#if defined _FSF_G95
      integer, pointer :: dum_ptr ! dummy pointer used only before and
                                  ! after calls to 'g95_show_locus'
#endif

   !------ end of declarations -- execution starts hereafter  ------

      if( mf_message_level /= 0 ) then

#if defined _INTEL_IFC
#ifndef _RELEASE
         if( mf_traceback_level == 2 .or.                               &
             ( mf_traceback_level == 1 .and. pause == "yes" ) ) then
            call beepqq( frequency=440, duration=200 )
            write(STDERR,*)
            call tracebackqq( string="  traceback requested from MUESLI :", &
                              user_exit_code=-1 )
            write(STDERR,*)
         else
            write(STDERR,*) "[traceback disabled]"
         end if
#else
         write(STDERR,*) "[traceback disabled]"
#endif
#else
#if defined _FSF_G95
#ifndef _RELEASE
         if( mf_traceback_level == 2 .or.                               &
             ( mf_traceback_level == 1 .and. pause == "yes" ) ) then
            write(STDERR,*)
            write(STDERR,"(A)") "  traceback requested from MUESLI :"
            call flush(STDERR)
            ! in order to be correct, the statement
            ! 'call g95_show_locus()' must be framed by any
            ! alloc/dealloc.
            allocate(dum_ptr); call g95_show_locus(); deallocate(dum_ptr)
            write(STDERR,*)
         else
            write(STDERR,*) "[traceback disabled]"
         end if
#else
         write(STDERR,*) "[traceback disabled]"
#endif
#else
#if defined _IBM_XLF
#ifndef _RELEASE
         if( mf_traceback_level == 2 .or.                               &
             ( mf_traceback_level == 1 .and. pause == "yes" ) ) then
            write(STDERR,*)
            write(STDERR,"(A)") "  traceback requested from MUESLI :"
            flush(STDERR)
            ! traceback
            call xl__trbk( )
         else
            write(STDERR,*) "[traceback disabled]"
         end if
#else
         write(STDERR,*) "[traceback disabled]"
#endif
#else
#if defined _GNU_GFC
#ifndef _RELEASE
#ifdef _HAVE_SHOW_BACKTRACE
         if( mf_traceback_level == 2 .or.                               &
             ( mf_traceback_level == 1 .and. pause == "yes" ) ) then
            write(STDERR,*)
            write(STDERR,"(A)") "  traceback requested from MUESLI :"
            call flush(STDERR)
            ! traceback
            call show_backtrace()
            write(STDERR,*)
         else
            write(STDERR,*) "[traceback disabled]"
         end if
#else
         write(STDERR,*) "[traceback disabled]"
#endif
#endif
#else
         write(STDERR,*) "[traceback disabled]"
#endif
#endif
#endif
#endif
      end if

      if( pause == "yes" .and. mf_message_level /= 0 ) then
         write(STDERR,*)
         write(STDERR,*) "(type [RETURN] to resume)"
         read *
         write(STDERR,*)
      end if

   end subroutine muesli_trace
!_______________________________________________________________________
!
end module mod_mfdebug
