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
    use :: stdlib_logger, only:stdlib_logger_type => logger_type
    use :: catechin_type_enum_logPurpose
    implicit none
    private
    public :: logger_selector

    !! Catechin declares 4 loggers for
    !! trace, report, development, and monitoring.
    !! Each of them is called a **purpose-specific logger**.
    !!
    !! purpose-specific loggers are public
    !! bacause it is used in tests to create expected results.
    !! It is unnecessary to be public if you delete the tests or
    !! add a new logger without unit tests.

    type(stdlib_logger_type), public, target :: trace
        !! logger for tracing operation status and execution order.
    type(stdlib_logger_type), public, target :: report
        !! logger for reporting (mainly on a screen) warnings, errors,
        !! and also normal tarminations.
    type(stdlib_logger_type), public, target :: develop
        !! logger for recording information during development.
    type(stdlib_logger_type), public, target :: monitor
        !! logger for monitoring internal status and configuration.

    !! Catechin defines enumerators to identify the purpose.
    !! The value of enumerators must not be assigned explicitly.
    !! That is, default values according to the language standard is used.

    enum, bind(c)
        enumerator :: Purpose_Trace
            !! an enumerator for specifying the logger used for trace.
        enumerator :: Purpose_Report
            !! an enumerator for specifying the logger used for report.
        enumerator :: Purpose_Develop
            !! an enumerator for specifying the logger used for develop.
        enumerator :: Purpose_Monitor
            !! an enumerator for specifying the logger used for monitoring.
        ! --put user defined enumerator above-- !
    end enum

    type, private :: log_purpose_enum
        type(log_purpose_enum_type), public :: Trace
        type(log_purpose_enum_type), public :: Report
        type(log_purpose_enum_type), public :: Develop
        type(log_purpose_enum_type), public :: Monitor
    end type log_purpose_enum

    type(log_purpose_enum), public, parameter :: &
        Pur = log_purpose_enum(Trace  =log_purpose_enum_type(Purpose_Trace  , "trace"), &
                               Report =log_purpose_enum_type(Purpose_Report , "report"), &
                               Develop=log_purpose_enum_type(Purpose_Develop, "develop"), &
                               Monitor=log_purpose_enum_type(Purpose_Monitor, "monitor") &
                               ) !&

    !!## adding a new purpose-specific logger
    !!
    !! A procedure to add a new purpose-specific logger is as follows:
    !!
    !!1.add logger as a module variable.
    !!
    !!```Fortran
    !!    type(stdlib_logger_type), public, target :: monitor
    !!        !! logger for monitoring internal status and configuration.
    !!
    !!    type(stdlib_logger_type), private, target :: support  ! <- add
    !!        !! logger for supporting users operation.  !
    !!```
    !!
    !!2.add enumerator
    !!
    !!```Fortran
    !!    enumerator :: Purpose_Monitor
    !!        !! an enumerator for specifying the logger used for monitoring.
    !!
    !!    enumerator :: Purpose_Support  ! <- add
    !!    ! --put user defined enumerator above-- !
    !!end enum
    !!```
    !!
    !!3.add a component to `log_purpose_enum`
    !!
    !!```Fortran
    !!type, private :: log_purpose_enum
    !!    type(log_purpose_enum_type), public :: Trace
    !!    type(log_purpose_enum_type), public :: Report
    !!    type(log_purpose_enum_type), public :: Develop
    !!    type(log_purpose_enum_type), public :: Monitor
    !!    type(log_purpose_enum_type), public :: Support ! <- add
    !!end type log_purpose_enum
    !!```
    !!
    !!4.add an argument to `log_purpose_enum` build-in constrcutor
    !!
    !!```Fortran
    !!type(log_purpose_enum), public, parameter :: &
    !!    Pur = log_purpose_enum(Trace  =log_purpose_enum_type(Purpose_Trace  , "trace"), &
    !!                           Report =log_purpose_enum_type(Purpose_Report , "report"), &
    !!                           Develop=log_purpose_enum_type(Purpose_Develop, "develop"), &
    !!                           Monitor=log_purpose_enum_type(Purpose_Monitor, "monitor") &
    !!                           Support=log_purpose_enum_type(Purpose_Support, "support") & ! <- add
    !!                           ) !&
    !!```
    !!
    !!5.add a case in [[logger_selector]]
    !!
    !!```Fortran
    !!    case (Pur%Monitor%enum)
    !!        logger => monitor
    !!
    !!    case (Pur%Support%enum) ! <- add
    !!        logger => support  !
    !!
    !!    case default
    !!        logger => null()
    !!```

contains
    !>Returns a pointer associated with a purpose-specific logger
    !>based on the argument `purpose`.
    !>
    !>Returns `null` when passed an unexpected actual argument `purpose`.
    function logger_selector(purpose) result(logger)
        implicit none
        !&<
        type(log_purpose_enum_type), intent(in) :: purpose
            !! enumerator for specifying the purpose
        !&>
        type(stdlib_logger_type), pointer :: logger
            !! a pointer to a purpose-specific logger

        select case (purpose%enum)
        case (Pur%Trace%enum)
            logger => trace

        case (Pur%Report%enum)
            logger => report

        case (Pur%Develop%enum)
            logger => develop

        case (Pur%Monitor%enum)
            logger => monitor

        case default
            logger => null()
        end select
    end function logger_selector
end module catechin_userDefinedLogger
