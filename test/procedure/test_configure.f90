module test_mod_configure
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:stdlib_logger_type => logger_type, success, &
        debug_level, information_level, warning_level, error_level
    use :: stdlib_strings
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:occurred
    use :: catechin_userDefinedLogger
    use :: catechin_procedure_logMessage
    use :: catechin_procedure_configure
    use :: catechin_type_enum_logLevel
    implicit none
    private
    public :: collect

contains
    !>constructing collection of tests.
    subroutine collect(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("configure level.", &
                                  test_configure_level) &
                     , new_unittest("configure timestamp.", &
                                    test_configure_timestamp) &
                     , new_unittest("configure filename.", &
                                    test_configure_filename) &
                     , new_unittest("configure stdout.", &
                                    test_configure_filename_stdout) &
                     ]
    end subroutine collect

    subroutine test_configure_level(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger

        trace_logger: block
            logger => logger_selector(Purpose_Trace)

            call configure(Purpose_Trace, level=Lv%DEBUG)
            call test_configure_purpose_level(Purpose_Trace, Lv%DEBUG, debug_level, "debug_level", error)
            if (occurred(error)) return

            call configure(Purpose_Trace, Lv%INFO)
            call test_configure_purpose_level(Purpose_Trace, Lv%INFO, information_level, "information_level", error)
            if (occurred(error)) return

            call configure(Purpose_Trace, Lv%WARN)
            call test_configure_purpose_level(Purpose_Trace, Lv%WARN, warning_level, "warning_level", error)
            if (occurred(error)) return

            call configure(Purpose_Trace, Lv%ERROR)
            call test_configure_purpose_level(Purpose_Trace, Lv%ERROR, error_level, "error_level", error)
            if (occurred(error)) return
        end block trace_logger

        report_logger: block
            logger => logger_selector(Purpose_Report)

            call configure(Purpose_Report, level=Lv%DEBUG)
            call test_configure_purpose_level(Purpose_Report, Lv%DEBUG, debug_level, "debug_level", error)
            if (occurred(error)) return

            call configure(Purpose_Report, Lv%INFO)
            call test_configure_purpose_level(Purpose_Report, Lv%INFO, information_level, "information_level", error)
            if (occurred(error)) return

            call configure(Purpose_Report, Lv%WARN)
            call test_configure_purpose_level(Purpose_Report, Lv%WARN, warning_level, "warning_level", error)
            if (occurred(error)) return

            call configure(Purpose_Report, Lv%ERROR)
            call test_configure_purpose_level(Purpose_Report, Lv%ERROR, error_level, "error_level", error)
            if (occurred(error)) return
        end block report_logger

        develop_logger: block
            logger => logger_selector(Purpose_Develop)

            call configure(Purpose_Develop, level=Lv%DEBUG)
            call test_configure_purpose_level(Purpose_Develop, Lv%DEBUG, debug_level, "debug_level", error)
            if (occurred(error)) return

            call configure(Purpose_Develop, Lv%INFO)
            call test_configure_purpose_level(Purpose_Develop, Lv%INFO, information_level, "information_level", error)
            if (occurred(error)) return

            call configure(Purpose_Develop, Lv%WARN)
            call test_configure_purpose_level(Purpose_Develop, Lv%WARN, warning_level, "warning_level", error)
            if (occurred(error)) return

            call configure(Purpose_Develop, Lv%ERROR)
            call test_configure_purpose_level(Purpose_Develop, Lv%ERROR, error_level, "error_level", error)
            if (occurred(error)) return
        end block develop_logger

        monitor_logger: block
            logger => logger_selector(Purpose_Monitor)

            call configure(Purpose_Monitor, level=Lv%DEBUG)
            call test_configure_purpose_level(Purpose_Monitor, Lv%DEBUG, debug_level, "debug_level", error)
            if (occurred(error)) return

            call configure(Purpose_Monitor, Lv%INFO)
            call test_configure_purpose_level(Purpose_Monitor, Lv%INFO, information_level, "information_level", error)
            if (occurred(error)) return

            call configure(Purpose_Monitor, Lv%WARN)
            call test_configure_purpose_level(Purpose_Monitor, Lv%WARN, warning_level, "warning_level", error)
            if (occurred(error)) return

            call configure(Purpose_Monitor, Lv%ERROR)
            call test_configure_purpose_level(Purpose_Monitor, Lv%ERROR, error_level, "error_level", error)
            if (occurred(error)) return
        end block monitor_logger
    contains
        subroutine test_configure_purpose_level(purpose, level, expected_level, level_name, error)
            implicit none
            integer(int32), intent(in) :: purpose
            type(log_level_enum_type), intent(in) :: level
            integer(int32) :: expected_level
            character(*), intent(in) :: level_name
            type(error_type), allocatable, intent(out) :: error

            integer(int32) :: acctual_level

            call logger%configuration(level=acctual_level)
            call check(error, acctual_level, expected_level, &
                       message="Configured '"//get_purpose_in_string(purpose)// &
                       "' logger threshold level '"//level%as_string()// &
                       "' is not `"//level_name//"`.")
        end subroutine test_configure_purpose_level
    end subroutine test_configure_level

    subroutine test_configure_timestamp(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger

        trace_logger: block
            logger => logger_selector(Purpose_Trace)

            call configure(Purpose_Trace, timestamp=.true.)
            call test_configure_purpose_timestamp(Purpose_Trace, .true., error)
            if (occurred(error)) return

            call configure(Purpose_Trace, timestamp=.false.)
            call test_configure_purpose_timestamp(Purpose_Trace, .false., error)
            if (occurred(error)) return
        end block trace_logger

        report_logger: block
            logger => logger_selector(Purpose_Report)

            call configure(Purpose_Report, timestamp=.true.)
            call test_configure_purpose_timestamp(Purpose_Trace, .true., error)
            if (occurred(error)) return

            call configure(Purpose_Report, timestamp=.false.)
            call test_configure_purpose_timestamp(Purpose_Report, .false., error)
            if (occurred(error)) return
        end block report_logger

        develop_logger: block
            logger => logger_selector(Purpose_Develop)

            call configure(Purpose_Develop, timestamp=.true.)
            call test_configure_purpose_timestamp(Purpose_Develop, .true., error)
            if (occurred(error)) return

            call configure(Purpose_Develop, timestamp=.false.)
            call test_configure_purpose_timestamp(Purpose_Develop, .false., error)
            if (occurred(error)) return
        end block develop_logger

        monitor_logger: block
            logger => logger_selector(Purpose_Monitor)

            call configure(Purpose_Monitor, timestamp=.true.)
            call test_configure_purpose_timestamp(Purpose_Monitor, .true., error)
            if (occurred(error)) return

            call configure(Purpose_Monitor, timestamp=.false.)
            call test_configure_purpose_timestamp(Purpose_Monitor, .false., error)
            if (occurred(error)) return
        end block monitor_logger
    contains
        subroutine test_configure_purpose_timestamp(purpose, expected, error)
            integer(int32), intent(in) :: purpose
            logical, intent(in) :: expected
            type(error_type), allocatable, intent(out) :: error
            logical :: actual

            call logger%configuration(time_stamp=actual)
            call check(error, actual, expected, &
                       message="Configured '"//get_purpose_in_string(purpose)// &
                       "' logger time stamp output '"//to_string(actual)// &
                       "' is not `"//to_string(expected)//"`.")
            if (occurred(error)) return
        end subroutine test_configure_purpose_timestamp
    end subroutine test_configure_timestamp

    subroutine test_configure_filename(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger

        trace_logger: block
            logger => logger_selector(Purpose_Trace)

            call configure(Purpose_Trace, filename="trace.log")
            call test_configure_purpose_filename(Purpose_Trace, "trace.log", error)
            if (occurred(error)) return

            call configure(Purpose_Trace, filename="trace.log", stdout=.false.)
            call test_configure_purpose_filename(Purpose_Trace, "trace.log", error)
            if (occurred(error)) return
        end block trace_logger

        report_logger: block
            logger => logger_selector(Purpose_Report)

            call configure(Purpose_Report, filename="report.log")
            call test_configure_purpose_filename(Purpose_Report, "report.log", error)
            if (occurred(error)) return

            call configure(Purpose_Report, filename="report.log", stdout=.false.)
            call test_configure_purpose_filename(Purpose_Report, "report.log", error)
            if (occurred(error)) return
        end block report_logger

        develop_logger: block
            logger => logger_selector(Purpose_Develop)

            call configure(Purpose_Develop, filename="devel.log")
            call test_configure_purpose_filename(Purpose_Develop, "devel.log", error)
            if (occurred(error)) return

            call configure(Purpose_Develop, filename="devel.log", stdout=.false.)
            call test_configure_purpose_filename(Purpose_Develop, "devel.log", error)
            if (occurred(error)) return
        end block develop_logger

        monitor_logger: block
            logger => logger_selector(Purpose_Monitor)

            call configure(Purpose_Monitor, filename="monitor.log")
            call test_configure_purpose_filename(Purpose_Monitor, "monitor.log", error)
            if (occurred(error)) return

            call configure(Purpose_Monitor, filename="monitor.log", stdout=.false.)
            call test_configure_purpose_filename(Purpose_Monitor, "monitor.log", error)
            if (occurred(error)) return
        end block monitor_logger
    contains
        subroutine test_configure_purpose_filename(purpose, filename_expected, error)
            implicit none
            integer(int32), intent(in) :: purpose
            character(*), intent(in) :: filename_expected
            type(error_type), allocatable, intent(out) :: error

            integer(int32), allocatable :: units(:)
            character(128) :: filename
            logical :: opened

            call logger%configuration(log_units=units)
            check_num_units: block
                call check(error, logger%log_units_assigned(), 1, &
                           message=get_purpose_in_string(purpose)//"did not opened log file "// &
                           "or opened unexpectedly some files or units.")
                if (occurred(error)) return
            end block check_num_units

            check_filename: block
                inquire (unit=units(1), opened=opened, name=filename)
                call check(error, trim(filename), filename_expected, &
                           message="log filename "//trim(filename)//" is not '"//filename_expected//"'")
                if (occurred(error)) return
            end block check_filename

            teardown: block
                integer(int32) :: unit
                if (opened) then
                    call logger%remove_log_unit(units(1), close_unit=.true.)
                end if
                open (newunit=unit, file=trim(filename))
                close (unit, status="delete")
            end block teardown
        end subroutine test_configure_purpose_filename
    end subroutine test_configure_filename

    subroutine test_configure_filename_stdout(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger

        trace_logger: block
            logger => logger_selector(Purpose_Trace)

            call configure(Purpose_Trace, filename="trace.log", stdout=.true.)
            call test_configure_purpose_filename_stdout(Purpose_Trace, "trace.log", error)
        end block trace_logger

        report_logger: block
            logger => logger_selector(Purpose_Report)

            call configure(Purpose_Report, filename="report.log", stdout=.true.)
            call test_configure_purpose_filename_stdout(Purpose_Report, "report.log", error)
        end block report_logger

        develop_logger: block
            logger => logger_selector(Purpose_Develop)

            call configure(Purpose_Develop, filename="devel.log", stdout=.true.)
            call test_configure_purpose_filename_stdout(Purpose_Develop, "devel.log", error)
        end block develop_logger

        monitor_logger: block
            logger => logger_selector(Purpose_Monitor)

            call configure(Purpose_Monitor, filename="monitor.log", stdout=.true.)
            call test_configure_purpose_filename_stdout(Purpose_Monitor, "monitor.log", error)
        end block monitor_logger
    contains
        subroutine test_configure_purpose_filename_stdout(purpose, filename_expected, error)
            implicit none
            integer(int32), intent(in) :: purpose
            character(*), intent(in) :: filename_expected
            type(error_type), allocatable, intent(out) :: error

            integer(int32), allocatable :: units(:)
            character(128) :: filename
            logical :: opened

            call logger%configuration(log_units=units)

            check_num_units: block
                call check(error, logger%log_units_assigned(), 2, &
                           message=get_purpose_in_string(purpose)//"did not opened log file "// &
                           "or opened unexpectedly some files or units.")
                if (occurred(error)) return
            end block check_num_units

            check_unit_number: block
                call check(error, count(units == output_unit), 1, &
                           message="`output_unit` not added.")
            end block check_unit_number

            check_filename: block
                inquire (unit=units(1), opened=opened, name=filename)
                call check(error, trim(filename), filename_expected, &
                           message="log filename "//trim(filename)//" is not '"//filename_expected//"'")
                if (occurred(error)) return
            end block check_filename

            teardown: block
                integer(int32) :: unit
                if (opened) then
                    call logger%remove_log_unit(units(1), close_unit=.true.)
                end if
                open (newunit=unit, file=trim(filename))
                close (unit, status="delete")
            end block teardown
        end subroutine test_configure_purpose_filename_stdout
    end subroutine test_configure_filename_stdout
end module test_mod_configure

program test_configure
    use, intrinsic :: iso_fortran_env
    use :: test_mod_configure
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("catechin_procedure % configure", collect) &
                  ]
    call run_test(test_suites)
end program test_configure
