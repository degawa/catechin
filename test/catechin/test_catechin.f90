module test_mod_catechin
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:stdlib_logger_type => logger_type, success, debug_level
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:occurred
    use :: catechin_procedure_logMessage
    use :: catechin_userDefinedLogger
    use :: catechin
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
                     new_unittest("`catechin_logging` writes log messages &
                                  &with the prefix `LEVEL: [trace]: [category]: `.", &
                                    test_logging_trace) &
                     , new_unittest("`catechin_logging` writes log messages &
                                  &with the prefix `LEVEL: [report]: [category]: `.", &
                                    test_logging_report) &
                     , new_unittest("`catechin_logging` writes log messages &
                                  &with the prefix `LEVEL: [develop]: [category]: `.", &
                                    test_logging_develop) &
                     , new_unittest("`catechin_logging` writes log messages &
                                  &with the prefix `LEVEL: [monitor]: [category]: `.", &
                                    test_logging_monitor) &
                     ]
    end subroutine collect

    !>test the procedure `[[catechin(module):logging_w_args(subroutine)]]`.
    !>
    !>This test is checking
    !>
    !>- the `[[catechin(module):logging_w_args(subroutine)]]`
    !> used with the trace logger writes
    !> a message prepended `LEVEL: [trace]: [category]: ` to an added log file.
    !>
    subroutine test_logging_trace(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call test_logging_w_args(Lv%DEBUG, "vtr_file%initialize()", &
                                 Pur%Trace, "IO.output.vtr", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%INFO, "json_file%initialize()", &
                                 Pur%Trace, "IO.input.json.configuration", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%WARN, "json_file%load()", &
                                 Pur%Trace, "IO.input.json.configuration", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%ERROR, "json_file%destroy()", &
                                 Pur%Trace, "IO.input.json.finalize", error)
        if (occurred(error)) return
    end subroutine test_logging_trace

    !>test the procedure `[[catechin(module):logging_w_args(subroutine)]]`.
    !>
    !>This test is checking
    !>
    !>- the `[[catechin(module):logging_w_args(subroutine)]]`
    !> used with the report logger writes
    !> a message prepended `LEVEL: [report]: [category]: ` to an added log file.
    !>
    subroutine test_logging_report(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call test_logging_w_args(Lv%DEBUG, "set log output level DEBUG", &
                                 Pur%Report, "System.logging", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%INFO, "326/10000 mas_tol=0.999 ite=256 divU=1d-7", &
                                 Pur%Report, "Simulation.core.progress", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%WARN, "could not output 'velocity000326.vtr'", &
                                 Pur%Report, "Simulation.io.error.ignored", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%ERROR, "configuration file 'config.json' not found", &
                                 Pur%Report, "App.configuration", error)
        if (occurred(error)) return
    end subroutine test_logging_report

    !>test the procedure `[[catechin(module):logging_w_args(subroutine)]]`.
    !>
    !>This test is checking
    !>
    !>- the `[[catechin(module):logging_w_args(subroutine)]]`
    !> used with the develop logger writes
    !> a message prepended `LEVEL: [develop]: [category]: ` to an added log file.
    !>
    subroutine test_logging_develop(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call test_logging_w_args(Lv%DEBUG, "'system.logging.level' not found in the configure file. &
                                           &default value 'information' is used.", &
                                 Pur%Develop, "App.configuration.logging", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%INFO, "set grid point from 0 to 65", &
                                 Pur%Develop, "Simulation.core", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%WARN, "actual argument `viscosity` not presented. &
                                          &the viscous term is neglected.", &
                                 Pur%Develop, "Simulation.core", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%ERROR, "actual argumet `density` not presented. &
                                           &cound not solve pressure Poisson equation.", &
                                 Pur%Develop, "Simulation.core", error)
        if (occurred(error)) return
    end subroutine test_logging_develop

    !>test the procedure `[[catechin(module):logging_w_args(subroutine)]]`.
    !>
    !>This test is checking
    !>
    !>- the `[[catechin(module):logging_w_args(subroutine)]]`
    !> used with the monitor logger writes
    !> a message prepended `LEVEL: [monitor]: [category]: ` to an added log file.
    !>
    subroutine test_logging_monitor(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call test_logging_w_args(Lv%DEBUG, "image 1 received 'velocity%x(1:100,1:100,50)' (100*100*8Byte) from image 2", &
                                 Pur%Monitor, "Comm.sent.async", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%INFO, "allocated 'velocity%x' (256MB)", &
                                 Pur%Monitor, "device.memory", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%WARN, "Few memories left >100MB", &
                                 Pur%Monitor, "device.memory", error)
        if (occurred(error)) return

        call test_logging_w_args(Lv%ERROR, "No space available to output velocity01000.vtr", &
                                 Pur%Monitor, "device.disk", error)
        if (occurred(error)) return
    end subroutine test_logging_monitor

    !>Write a log message to a file and read the message from the file.
    !>
    subroutine test_logging_w_args(level, message, purpose, category, error)
        use :: stdlib_ascii
        implicit none
        type(log_level_enum_type), intent(in) :: level
        character(*), intent(in) :: message
        type(log_purpose_enum_type), intent(in) :: purpose
        character(*), intent(in) :: category
        type(error_type), allocatable, intent(out) :: error

        type(stdlib_logger_type), pointer :: logger
        character(:), allocatable :: log_filename, log_msg, line
        integer(int32) :: log_unit
        integer(int32) :: range(2)

        ! status
        integer(int32) :: stat
        character(:), allocatable :: io_msg

        !|#### test procedure
        !1. select a specific-purpose logger
        logger => logger_selector(purpose)

        !!1. deactivete outputting timestamp and set threshold to debug level
        call logger%configure(time_stamp=.false., level=debug_level)

        !!1. set log filename
        log_filename = purpose%as_string()//".log"

        !!1. delete log file if it has already existed
        call delete_existing_log_file(log_filename, stat)
        call check(error, stat, success, &
                   message="Could not continue the test due to failing log file handling")
        if (occurred(error)) return

        !!1. add the log file to the logger
        call logger%add_log_file(filename=log_filename, unit=log_unit, stat=stat)
        call check(error, stat, success, message="Could not add log file")
        if (occurred(error)) return

        !!1. output log message to the log file<br>
        !!ex)<br> `DEBUG: [trace]: [IO.output.vtr]: vtr_file%initialize()`
        log_msg = message
        call logging(level, message, &
                     purpose=purpose, category=category)

        !!1. remove the log file unit with closing the unit
        call logger%remove_log_unit(log_unit, close_unit=.true.)

        !!1. open the log file
        open (newunit=log_unit, file=log_filename, action="read", position="rewind", iostat=stat)
        call check(error, stat, success, &
                   message="Could not open the log file "//log_filename)
        if (occurred(error)) return

        !!1. read log message written in the log file
        allocate_str: block
            integer(int32) :: length
            length = len(level%as_string()//": ") &
                     + len("["//purpose%as_string()//"]: ") &
                     + len("["//category//"]: ") &
                     + len(log_msg)

            allocate (character(length) :: line)
            allocate (character(256) :: io_msg)
        end block allocate_str

        read (log_unit, '(A)', iostat=stat, iomsg=io_msg) line
        call check(error, stat, success, &
                   message="Could not read the log file "//log_filename &
                   //" with error message "//trim(io_msg))
        if (occurred(error)) return

        !!1. compare the log level<br>
        !!ex)<br>
        !!`.123456`<br>
        !!`"DEBUG: [trace]: [IO.output.vtr]: vtr_file%initialize()" == to_upper("debug")`<br>
        !!`.^---^`
        range(1) = 1
        range(2) = len(level%as_string())
        call check(error, line(range(1):range(2)), to_upper(level%as_string()), &
                   message="written log level and read log level are different")
        if (occurred(error)) return

        !!1. compare the purpose of logging<br>
        !!ex)<br>
        !!`.123456789012345678901234567890123456789012345678901234`<br>
        !!`"DEBUG: [trace]: [IO.output.vtr]: vtr_file%initialize()" == "trace"`<br>
        !!`.........^---^`
        range(1) = range(2) + len(": ") + len("[") + 1
        range(2) = range(1) - 1 + len(purpose%as_string())
        call check(error, line(range(1):range(2)), purpose%as_string(), &
                   message="written log purpose and read log puprose are different")
        if (occurred(error)) return

        !!1. compare the log category<br>
        !!ex)<br>
        !!`.123456789012345678901234567890123456789012345678901234`<br>
        !!`"DEBUG: [trace]: [IO.output.vtr]: vtr_file%initialize()" == "IO.output.vtr"`<br>
        !!`..................^-----------^`
        range(1) = range(2) + len("]: ") + len("[") + 1
        range(2) = range(1) - 1 + len(category)
        call check(error, line(range(1):range(2)), category, &
                   message="written log category and read log category are different")
        if (occurred(error)) return

        !!1. compare the log category<br>
        !!ex)<br>
        !!`.123456789012345678901234567890123456789012345678901234`<br>
        !!`"DEBUG: [trace]: [IO.output.vtr]: vtr_file%initialize()" == "vtr_file%initialize()"`<br>
        !!`..................................^-------------------^`
        range(1) = range(2) + len("]: ") + 1
        range(2) = range(1) - 1 + len(log_msg)
        call check(error, line(range(1):range(2)), log_msg, &
                   message="written message and read massage are different")
        if (occurred(error)) return

        !!1. close and delete the log file
        close (log_unit, status="delete", iostat=stat)
        call check(error, stat, success, &
                   message="Could not delete the log file "//log_filename)
        if (occurred(error)) return

        deallocate (log_filename)
        deallocate (line)
        deallocate (log_msg)
        logger => null()
    contains
        !>delete existing log file using close(unit, status="delete")
        subroutine delete_existing_log_file(name, io_stat)
            implicit none
            character(*), intent(in) :: name
            integer(int32), intent(out) :: io_stat

            integer(int32) :: unit
            logical :: exists

            inquire (file=name, exist=exists)
            if (exists) then
                ! open the existing log file once to delete using close()
                open (newunit=unit, file=name, status="old", iostat=io_stat)

                if (io_stat == success) then
                    close (unit, status="delete", iostat=io_stat)
                end if
            else
                io_stat = success
            end if
        end subroutine delete_existing_log_file
    end subroutine test_logging_w_args
end module test_mod_catechin

program test_catechin
    use, intrinsic :: iso_fortran_env
    use :: test_mod_catechin
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("catechin % logging_w_args", collect) &
                  ]
    call run_test(test_suites)
end program test_catechin
