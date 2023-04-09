!>This module is used to refer to the elements,
!>parameters and procedures, constructing catechin.
!>
!>This module provides variables and procedures related to logging.
!>
!>The variables include
!>
!>- enumerators for specifying a purpose-specific logger
!>- parameters for specifiying a log level
!>
!>The procedures include
!>
!>- writing a log message with prefix including purpose, level, and category.
!>
module catechin
    use :: catechin_procedure
    use :: catechin_type
    use :: catechin_userDefinedLogger
    implicit none
    private
    public :: logging
    public :: configure
    public :: logger
    public :: Lv_DEBUG, Lv_INFO, Lv_WARN, Lv_ERROR
    public :: Purpose_Trace, Purpose_Report, Purpose_Develop, Purpose_Monitor

    interface logging
        procedure :: logging_w_args
        procedure :: logging_w_logger
    end interface

    interface logger
        procedure :: construct_logger_type
    end interface

contains
    subroutine logging_w_logger(logger)
        implicit none
        type(logger_type), intent(in) :: logger

        call logger%logging()
    end subroutine logging_w_logger

    !>Write a log massage with prefixes
    !>including log level, purpose, and category.
    !>
    !>This procedure is given a generic name `catechin__logging`
    !>using an interface.
    !>
    !>#### Example
    !>```Fortran
    !>program main
    !>    use :: catechin
    !>    implicit none
    !>
    !>    call catechin__logging(Lv_INFO, "vtr_file%initialize", purpose=Purpose_Trace, category="IO.vector3d.vtr")
    !>    ! YYYY-MM-DD hh:mm:ss.sss: INFO: [trace]: [IO.vector3d.vtr]: vtr_file%initialize
    !>end program main
    !>```
    !>
    subroutine logging_w_args(level, message, module, procedure, purpose, category)
        use, intrinsic :: iso_fortran_env
        implicit none
        !&<
        character(*)    , intent(in)            :: level
            !! log level
        character(*)    , intent(in)            :: message
            !! log message
        integer(int32)  , intent(in), optional  :: purpose
            !! purpose of logging
        character(*)    , intent(in), optional  :: category
            !! log category
        character(*)    , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of this procedure
        character(*)    , intent(in), optional  :: procedure
            !! a name of the procedure containing
            !! the current invocation of this procedure
        !&>
        character(:), allocatable :: prefix
            !! prefix containing level, purpose, and category
        procedure(Ilog_message), pointer :: log_message
            !! a pointer to logging procedure

        ! select log messaging procedure
        log_message => log_message_procedure_selector(level)
        if (.not. associated(log_message)) return

        ! constructing the prefix
        prefix = ""
        if (present(purpose)) prefix = prefix//"["//get_purpose_in_string(purpose)//"]"//": "
        if (present(category)) prefix = prefix//"["//category//"]"//": "

        ! write log massage with specifing purpose-specific logger, messages with prefix,
        ! module name, and purocedure name.
        call log_message(logger_selector(purpose), prefix//message, module, procedure)
    end subroutine logging_w_args

    function construct_logger_type(purpose, level, category) result(logger)
        use, intrinsic :: iso_fortran_env
        implicit none
        !&<
        integer(int32), intent(in) :: purpose
            !! logging purpose
        character(*), intent(in) :: level
            !! log level
        character(*), intent(in) :: category
            !! log category
        !&>
        type(logger_type) :: logger
        procedure(Ilog_message), pointer :: log_message

        ! call logger%set_log_message_proc(log_message_procedure_factory(level))
        ! leads a compile error
        log_message => log_message_procedure_selector(level)

        call logger%set_logger(logger_selector(purpose))
        call logger%set_log_message_proc(log_message)
        call logger%set_purpose(get_purpose_in_string(purpose))
        call logger%set_category(category)
    end function construct_logger_type

    !>Returns log messaging procedure based on the argument `level`.
    !>
    !>Returns `null` when passed an unexpected actual argument `level`
    !>
    !>@warning
    !>This function must be placed in the same module
    !>as procedures, `[[logging_w_args]]`,
    !>referring to it to avoid compile errors in gfortran and NAG Fortran.
    !>
    !>Intel Fortran compiles successfully even if this function
    !>is in a different module.
    !>Suppose other compilers can compile successfully in the future,
    !>I will move this function to `[[catechin_procedure_logMessage]]`,
    !> and will change the log messaging functions referred to
    !> in this function to private.
    !>@endwarning
    !>
    function log_message_procedure_selector(level) result(log_message)
        use :: catechin_procedure_logMessage
        implicit none
        character(*), intent(in) :: level
        procedure(Ilog_message), pointer :: log_message

        select case (level)
        case (Lv_DEBUG)
            log_message => log_debug

        case (Lv_INFO)
            log_message => log_info

        case (Lv_WARN)
            log_message => log_warn

        case (Lv_ERROR)
            log_message => log_error

        case default
            log_message => null()
        end select
    end function log_message_procedure_selector
end module catechin
