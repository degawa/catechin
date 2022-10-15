module catechin_userDefinedLogger
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: stdlib_logger, only:logger_type
    implicit none
    private
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
end module catechin_userDefinedLogger
