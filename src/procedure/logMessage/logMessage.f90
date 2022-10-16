!>This module provides variables and procedures
!>related to log writing procedures.
!>
!>The variables include
!>
!>- specifiers for log writing procedures with different log level
!>
!>The procedures include
!>
!>- wrappers for type-bound procedures in `logger_type`
!>
module catechin_procedure_logMessage
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:logger_type
    implicit none
    private
    public :: Ilog_message
    public :: log_debug
    public :: log_info
    public :: log_warn
    public :: log_error

    !!The procedure specifiers are string constants,
    !!unlike the level definitions in the stdlib_logger.
    !!
    !!I considered using the level definition as follows:
    !!
    !!```Fortran
    !! integer(int32), public, parameter :: Lv_DEBUG = debug_level
    !! integer(int32), public, parameter :: Lv_INFO = information_level
    !! integer(int32), public, parameter :: Lv_WARN = warning_level
    !! integer(int32), public, parameter :: Lv_ERROR = error_level
    !!```
    !!
    !!I finally stopped using above specifiler due to
    !! internal compile errors in gfortran and NAG Fortran
    !! at association a procedure pointer with returned value
    !! from [[log_message_procedure_selector]].
    !!

    character(*), public, parameter :: Lv_DEBUG = "debug"
        !! specifier for the log_debug procedure
    character(*), public, parameter :: Lv_INFO = "info"
        !! specifier for the log_information procedure
    character(*), public, parameter :: Lv_WARN = "warn"
        !! specifier for the log_warning procedure
    character(*), public, parameter :: Lv_ERROR = "error"
        !! specifier for the log_error procedure

    interface
        !>interface to type-bound procedures in logger_type
        subroutine Ilog_message(logger, message, module, procedure)
            use :: stdlib_logger, only:logger_type
            !&<
            class(logger_type)  , intent(in)            :: logger
            character(*)        , intent(in)            :: message
            character(*)        , intent(in), optional  :: module
            character(*)        , intent(in), optional  :: procedure
            !&>
        end subroutine Ilog_message
    end interface

contains
    !>Write a log message with the prefix `DEBUG: `.
    !>
    !>The timestamp is prepended if the timestamp writing is activated by
    !>`logger_type%configure()`.
    !>Module and procedure names are also prepended if these are presented.
    subroutine log_debug(logger, message, module, procedure)
        implicit none
        !&<
        class(logger_type)  , intent(in)            :: logger
            !! logger used to write log message
        character(*)        , intent(in)            :: message
            !! log message
        character(*)        , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of `log_debug`
        character(*)        , intent(in), optional  :: procedure
            !! a name of the procedure containing
            !! the current invocation of `log_debug`
        !&>
        call logger%log_debug(message, module=module, procedure=procedure)
    end subroutine log_debug

    !>Write a log message with the prefix `INFO: `.
    !>
    !>The timestamp is prepended if the timestamp writing is activated by
    !>`logger_type%configure()`.
    !>Module and procedure names are also prepended if these are presented.
    subroutine log_info(logger, message, module, procedure)
        implicit none
        !&<
        class(logger_type)  , intent(in)            :: logger
            !! logger used to write log message
        character(*)        , intent(in)            :: message
            !! log message
        character(*)        , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of `log_information`
        character(*)        , intent(in), optional  :: procedure
            !! a name of the procedure containing
            !! the current invocation of `log_information`
        !&>
        call logger%log_information(message, module=module, procedure=procedure)
    end subroutine log_info

    !>Write a log message with the prefix `WARN: `.
    !>
    !>The timestamp is prepended if the timestamp writing is activated by
    !>`logger_type%configure()`.
    !>Module and procedure names are also prepended if these are presented.
    subroutine log_warn(logger, message, module, procedure)
        implicit none
        !&<
        class(logger_type)  , intent(in)            :: logger
            !! logger used to write log message
        character(*)        , intent(in)            :: message
            !! log message
        character(*)        , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of `log_warn`
        character(*)        , intent(in), optional  :: procedure
            !! a name of the procedure containing
            !! the current invocation of `log_warn`
        !&>
        call logger%log_warning(message, module=module, procedure=procedure)
    end subroutine log_warn

    !>Write a log message with the prefix `ERROR: `.
    !>
    !>The timestamp is prepended if the timestamp writing is activated by
    !>`logger_type%configure()`.
    !>Module and procedure names are also prepended if these are presented.
    subroutine log_error(logger, message, module, procedure)
        implicit none
        !&<
        class(logger_type)  , intent(in)            :: logger
            !! logger used to write log message
        character(*)        , intent(in)            :: message
            !! log message
        character(*)        , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of `log_error`
        character(*)        , intent(in), optional  :: procedure
            !! a name of the procedure containing
            !! the current invocation of `log_error`
        !&>
        call logger%log_error(message, module=module, procedure=procedure)
    end subroutine log_error
end module catechin_procedure_logMessage
