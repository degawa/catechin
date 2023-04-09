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
    use :: catechin_type_enum_logPurpose
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
            logger => logger_selector(Pur%Trace)

            call configure(Pur%Trace, level=Lv%DEBUG)
            call test_configure_purpose_level(Pur%Trace, Lv%DEBUG, debug_level, "debug_level", error)
            if (occurred(error)) return

            call configure(Pur%Trace, Lv%INFO)
            call test_configure_purpose_level(Pur%Trace, Lv%INFO, information_level, "information_level", error)
            if (occurred(error)) return

            call configure(Pur%Trace, Lv%WARN)
            call test_configure_purpose_level(Pur%Trace, Lv%WARN, warning_level, "warning_level", error)
            if (occurred(error)) return

            call configure(Pur%Trace, Lv%ERROR)
            call test_configure_purpose_level(Pur%Trace, Lv%ERROR, error_level, "error_level", error)
            if (occurred(error)) return
        end block trace_logger

        report_logger: block
            logger => logger_selector(Pur%Report)

            call configure(Pur%Report, level=Lv%DEBUG)
            call test_configure_purpose_level(Pur%Report, Lv%DEBUG, debug_level, "debug_level", error)
            if (occurred(error)) return

            call configure(Pur%Report, Lv%INFO)
            call test_configure_purpose_level(Pur%Report, Lv%INFO, information_level, "information_level", error)
            if (occurred(error)) return

            call configure(Pur%Report, Lv%WARN)
            call test_configure_purpose_level(Pur%Report, Lv%WARN, warning_level, "warning_level", error)
            if (occurred(error)) return

            call configure(Pur%Report, Lv%ERROR)
            call test_configure_purpose_level(Pur%Report, Lv%ERROR, error_level, "error_level", error)
            if (occurred(error)) return
        end block report_logger

        develop_logger: block
            logger => logger_selector(Pur%Develop)

            call configure(Pur%Develop, level=Lv%DEBUG)
            call test_configure_purpose_level(Pur%Develop, Lv%DEBUG, debug_level, "debug_level", error)
            if (occurred(error)) return

            call configure(Pur%Develop, Lv%INFO)
            call test_configure_purpose_level(Pur%Develop, Lv%INFO, information_level, "information_level", error)
            if (occurred(error)) return

            call configure(Pur%Develop, Lv%WARN)
            call test_configure_purpose_level(Pur%Develop, Lv%WARN, warning_level, "warning_level", error)
            if (occurred(error)) return

            call configure(Pur%Develop, Lv%ERROR)
            call test_configure_purpose_level(Pur%Develop, Lv%ERROR, error_level, "error_level", error)
            if (occurred(error)) return
        end block develop_logger

        monitor_logger: block
            logger => logger_selector(Pur%Monitor)

            call configure(Pur%Monitor, level=Lv%DEBUG)
            call test_configure_purpose_level(Pur%Monitor, Lv%DEBUG, debug_level, "debug_level", error)
            if (occurred(error)) return

            call configure(Pur%Monitor, Lv%INFO)
            call test_configure_purpose_level(Pur%Monitor, Lv%INFO, information_level, "information_level", error)
            if (occurred(error)) return

            call configure(Pur%Monitor, Lv%WARN)
            call test_configure_purpose_level(Pur%Monitor, Lv%WARN, warning_level, "warning_level", error)
            if (occurred(error)) return

            call configure(Pur%Monitor, Lv%ERROR)
            call test_configure_purpose_level(Pur%Monitor, Lv%ERROR, error_level, "error_level", error)
            if (occurred(error)) return
        end block monitor_logger
    contains
        subroutine test_configure_purpose_level(purpose, level, expected_level, level_name, error)
            implicit none
            type(log_purpose_enum_type), intent(in) :: purpose
            type(log_level_enum_type), intent(in) :: level
            integer(int32) :: expected_level
            character(*), intent(in) :: level_name
            type(error_type), allocatable, intent(out) :: error

            integer(int32) :: acctual_level

            call logger%configuration(level=acctual_level)
            call check(error, acctual_level, expected_level, &
                       message="Configured '"//purpose%as_string()// &
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
            logger => logger_selector(Pur%Trace)

            call configure(Pur%Trace, timestamp=.true.)
            call test_configure_purpose_timestamp(Pur%Trace, .true., error)
            if (occurred(error)) return

            call configure(Pur%Trace, timestamp=.false.)
            call test_configure_purpose_timestamp(Pur%Trace, .false., error)
            if (occurred(error)) return
        end block trace_logger

        report_logger: block
            logger => logger_selector(Pur%Report)

            call configure(Pur%Report, timestamp=.true.)
            call test_configure_purpose_timestamp(Pur%Trace, .true., error)
            if (occurred(error)) return

            call configure(Pur%Report, timestamp=.false.)
            call test_configure_purpose_timestamp(Pur%Report, .false., error)
            if (occurred(error)) return
        end block report_logger

        develop_logger: block
            logger => logger_selector(Pur%Develop)

            call configure(Pur%Develop, timestamp=.true.)
            call test_configure_purpose_timestamp(Pur%Develop, .true., error)
            if (occurred(error)) return

            call configure(Pur%Develop, timestamp=.false.)
            call test_configure_purpose_timestamp(Pur%Develop, .false., error)
            if (occurred(error)) return
        end block develop_logger

        monitor_logger: block
            logger => logger_selector(Pur%Monitor)

            call configure(Pur%Monitor, timestamp=.true.)
            call test_configure_purpose_timestamp(Pur%Monitor, .true., error)
            if (occurred(error)) return

            call configure(Pur%Monitor, timestamp=.false.)
            call test_configure_purpose_timestamp(Pur%Monitor, .false., error)
            if (occurred(error)) return
        end block monitor_logger
    contains
        subroutine test_configure_purpose_timestamp(purpose, expected, error)
            type(log_purpose_enum_type), intent(in) :: purpose
            logical, intent(in) :: expected
            type(error_type), allocatable, intent(out) :: error
            logical :: actual

            call logger%configuration(time_stamp=actual)
            call check(error, actual, expected, &
                       message="Configured '"//purpose%as_string()// &
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
            logger => logger_selector(Pur%Trace)

            call configure(Pur%Trace, filename="trace.log")
            call test_configure_purpose_filename(Pur%Trace, "trace.log", error)
            if (occurred(error)) return

            call configure(Pur%Trace, filename="trace.log", stdout=.false.)
            call test_configure_purpose_filename(Pur%Trace, "trace.log", error)
            if (occurred(error)) return
        end block trace_logger

        report_logger: block
            logger => logger_selector(Pur%Report)

            call configure(Pur%Report, filename="report.log")
            call test_configure_purpose_filename(Pur%Report, "report.log", error)
            if (occurred(error)) return

            call configure(Pur%Report, filename="report.log", stdout=.false.)
            call test_configure_purpose_filename(Pur%Report, "report.log", error)
            if (occurred(error)) return
        end block report_logger

        develop_logger: block
            logger => logger_selector(Pur%Develop)

            call configure(Pur%Develop, filename="devel.log")
            call test_configure_purpose_filename(Pur%Develop, "devel.log", error)
            if (occurred(error)) return

            call configure(Pur%Develop, filename="devel.log", stdout=.false.)
            call test_configure_purpose_filename(Pur%Develop, "devel.log", error)
            if (occurred(error)) return
        end block develop_logger

        monitor_logger: block
            logger => logger_selector(Pur%Monitor)

            call configure(Pur%Monitor, filename="monitor.log")
            call test_configure_purpose_filename(Pur%Monitor, "monitor.log", error)
            if (occurred(error)) return

            call configure(Pur%Monitor, filename="monitor.log", stdout=.false.)
            call test_configure_purpose_filename(Pur%Monitor, "monitor.log", error)
            if (occurred(error)) return
        end block monitor_logger
    contains
        subroutine test_configure_purpose_filename(purpose, filename_expected, error)
            implicit none
            type(log_purpose_enum_type), intent(in) :: purpose
            character(*), intent(in) :: filename_expected
            type(error_type), allocatable, intent(out) :: error

            integer(int32), allocatable :: units(:)
            character(128) :: filename
            logical :: opened

            call logger%configuration(log_units=units)
            check_num_units: block
                call check(error, logger%log_units_assigned(), 1, &
                           message=purpose%as_string()//"did not opened log file "// &
                           "or opened unexpectedly some files or units.")
                if (occurred(error)) return
            end block check_num_units

            check_filename: block
                integer(int32) :: pos
                character(1) :: separator
                inquire (unit=units(1), opened=opened, name=filename)

                ! ifort return filename with full path.
                ! searching a separator from backward and retrieve filename.
                separator = "\"
#if defined(__unix__) || (__linux__)
                separator = "/"
#endif
                pos = index(filename, separator, back=.true.) + 1
                call check(error, trim(filename(pos:)), filename_expected, &
                           message="log filename "//trim(filename(pos:))//" is not '"//filename_expected//"'")
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
            logger => logger_selector(Pur%Trace)

            call configure(Pur%Trace, filename="trace.log", stdout=.true.)
            call test_configure_purpose_filename_stdout(Pur%Trace, "trace.log", error)
        end block trace_logger

        report_logger: block
            logger => logger_selector(Pur%Report)

            call configure(Pur%Report, filename="report.log", stdout=.true.)
            call test_configure_purpose_filename_stdout(Pur%Report, "report.log", error)
        end block report_logger

        develop_logger: block
            logger => logger_selector(Pur%Develop)

            call configure(Pur%Develop, filename="devel.log", stdout=.true.)
            call test_configure_purpose_filename_stdout(Pur%Develop, "devel.log", error)
        end block develop_logger

        monitor_logger: block
            logger => logger_selector(Pur%Monitor)

            call configure(Pur%Monitor, filename="monitor.log", stdout=.true.)
            call test_configure_purpose_filename_stdout(Pur%Monitor, "monitor.log", error)
        end block monitor_logger
    contains
        subroutine test_configure_purpose_filename_stdout(purpose, filename_expected, error)
            implicit none
            type(log_purpose_enum_type), intent(in) :: purpose
            character(*), intent(in) :: filename_expected
            type(error_type), allocatable, intent(out) :: error

            integer(int32), allocatable :: units(:)
            character(128) :: filename
            logical :: opened

            call logger%configuration(log_units=units)

            check_num_units: block
                call check(error, logger%log_units_assigned(), 2, &
                           message=purpose%as_string()//"did not opened log file "// &
                           "or opened unexpectedly some files or units.")
                if (occurred(error)) return
            end block check_num_units

            check_unit_number: block
                call check(error, count(units == output_unit), 1, &
                           message="`output_unit` not added.")
            end block check_unit_number

            check_filename: block
                integer(int32) :: pos
                character(1) :: separator
                inquire (unit=units(1), opened=opened, name=filename)

                ! ifort return filename with full path.
                ! searching a separator from backward and retrieve filename.
                separator = "\"
#if defined(__unix__) || (__linux__)
                separator = "/"
#endif
                pos = index(filename, separator, back=.true.) + 1
                inquire (unit=units(1), opened=opened, name=filename)
                call check(error, trim(filename(pos:)), filename_expected, &
                           message="log filename "//trim(filename(pos:))//" is not '"//filename_expected//"'")
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
