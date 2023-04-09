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
    use :: stdlib_logger, only:stdlib_logger_type => logger_type, &
        debug_level, information_level, warning_level, error_level, none_level
    use :: catechin_type_enum_logLevel
    implicit none
    private
    public :: Ilog_message
    public :: log_debug
    public :: log_info
    public :: log_warn
    public :: log_error

    type, private :: log_level_enum
        type(log_level_enum_type), public :: DEBUG
        type(log_level_enum_type), public :: INFO
        type(log_level_enum_type), public :: WARN
        type(log_level_enum_type), public :: ERROR
    end type log_level_enum

    type(log_level_enum), public, parameter :: &
        Lv = log_level_enum(DEBUG=log_level_enum_type(debug_level       , "debug"), &
                            INFO =log_level_enum_type(information_level , "info"), &
                            WARN =log_level_enum_type(warning_level     , "warn"), &
                            ERROR=log_level_enum_type(error_level       , "error") &
                            ) !&

    interface
        !>interface to type-bound procedures in logger_type
        subroutine Ilog_message(logger, message, module, procedure)
            use :: stdlib_logger, only:stdlib_logger_type => logger_type
            !&<
            class(stdlib_logger_type)   , intent(in)            :: logger
            character(*)                , intent(in)            :: message
            character(*)                , intent(in), optional  :: module
            character(*)                , intent(in), optional  :: procedure
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
        class(stdlib_logger_type)   , intent(in)            :: logger
            !! logger used to write log message
        character(*)                , intent(in)            :: message
            !! log message
        character(*)                , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of `log_debug`
        character(*)                , intent(in), optional  :: procedure
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
        class(stdlib_logger_type)   , intent(in)            :: logger
            !! logger used to write log message
        character(*)                , intent(in)            :: message
            !! log message
        character(*)                , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of `log_information`
        character(*)                , intent(in), optional  :: procedure
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
        class(stdlib_logger_type)   , intent(in)            :: logger
            !! logger used to write log message
        character(*)                , intent(in)            :: message
            !! log message
        character(*)                , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of `log_warn`
        character(*)                , intent(in), optional  :: procedure
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
        class(stdlib_logger_type)   , intent(in)            :: logger
            !! logger used to write log message
        character(*)                , intent(in)            :: message
            !! log message
        character(*)                , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of `log_error`
        character(*)                , intent(in), optional  :: procedure
            !! a name of the procedure containing
            !! the current invocation of `log_error`
        !&>
        call logger%log_error(message, module=module, procedure=procedure)
    end subroutine log_error
end module catechin_procedure_logMessage
