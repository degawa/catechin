!>This module provides variables and procedures
!>related to pre-declared (default) loggers.
!>
!>The variables include
!>
!>- loggers for some specific purposes
!>- enumerators representing purposes
!>
!>The procedures include
!>
!>- select a logger based on a enumerator.
!>
module catechin_userDefinedLogger
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: stdlib_logger, only:logger_type
    implicit none
    private
    public :: logger_selector
    public :: get_purpose_in_string
    public :: Purpose_Trace, &
              Purpose_Report, &
              Purpose_Develop, &
              Purpose_Measure, &
              Purpose_Sentinel

    !! Catechin declares 4 loggers for
    !! trace, report, development, and measurement.
    !! Each of them is called **a purpose-specific logger**.

    type(logger_type), public, target :: trace
        !! logger for tracing operation status and execution order.
    type(logger_type), public, target :: report
        !! logger for reporting (mainly on a screen) warnings, errors,
        !! and also normal tarminations.
    type(logger_type), public, target :: develop
        !! logger for recording information during development.
    type(logger_type), public, target :: measure
        !! logger for measuring internal status and configuration.

    !! Catechin defines enumerators to specify purposes as arguments.
    !! The value of enumerators must not be assigned explicitly.
    !! That is, default values according to the language standard is used.

    enum, bind(c)
        enumerator :: Purpose_Trace
            !! an enumerator for specifying the logger used for trace.
        enumerator :: Purpose_Report
            !! an enumerator for specifying the logger used for report.
        enumerator :: Purpose_Develop
            !! an enumerator for specifying the logger used for develop.
        enumerator :: Purpose_Measure
            !! an enumerator for specifying the logger used for measurement.

        ! --put user defined enumerator above-- !
        enumerator :: Purpose_Sentinel
    end enum

contains
    !>Returns a pointer associated with a purpose-specific logger
    !>based on the argument `purpose`.
    !>
    !>Returns `null` when passed an unexpected actual argument `purpose`.
    function logger_selector(purpose) result(logger)
        implicit none
        !&<
        integer(int32), intent(in) :: purpose
            !! enumerator for specifying the purpose
        !&>
        type(logger_type), pointer :: logger
            !! a pointer to a purpose-specific logger

        select case (purpose)
        case (Purpose_Trace)
            logger => trace

        case (Purpose_Report)
            logger => report

        case (Purpose_Develop)
            logger => develop

        case (Purpose_Measure)
            logger => measure

        case default
            logger => null()
        end select
    end function logger_selector

    !>Returns purpose in string.
    !>
    !>Returns 0-length string when passed an unexpected actual argument `purpose`.
    function get_purpose_in_string(purpose) result(str)
        implicit none
        !&<
        integer(int32), intent(in) :: purpose
            !! enumerator for specifying a purpose
        !&>
        character(:), allocatable :: str
            !! a purpose name in a string

        select case (purpose)
        case (Purpose_Trace)
            str = "trace"

        case (Purpose_Report)
            str = "report"

        case (Purpose_Develop)
            str = "develop"

        case (Purpose_Measure)
            str = "measure"

        case default
            str = ""
        end select
    end function get_purpose_in_string
end module catechin_userDefinedLogger
