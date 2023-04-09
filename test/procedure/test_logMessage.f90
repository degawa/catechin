module test_mod_logMessage
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:stdlib_logger_type => logger_type, success, &
        debug_level, information_level, warning_level, error_level
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:occurred
    use :: catechin_userDefinedLogger
    use :: catechin_procedure_logMessage
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
                     new_unittest("`log_debug` procedure wrapping `logger_type%log_debug` &
                                  &writes log messages with the prefix `DEBUG: `.", &
                                    test_log_message_procedure_debug) &
                     , new_unittest("`log_info` procedure wrapping `logger_type%log_information` &
                                    &writes log messages with the prefix `INFO: `.", &
                                    test_log_message_procedure_info) &
                     , new_unittest("`log_warn` procedure wrapping `logger_type%log_warning` &
                                    &writes log messages with the prefix `WARN: `.", &
                                    test_log_message_procedure_warn) &
                     , new_unittest("`log_error`procedure wrapping`logger_type%log_error` &
                                    &writes log messages with the prefix`ERROR: `.", &
                                    test_log_message_procedure_error) &
                     ]
    end subroutine collect

    !>testing the procedure `[[catechin_procedure_logMessage(module):log_debug(subroutine)]]`
    !>with associating with a procedure pointer.
    !>
    !>This test is checking
    !>
    !>- a procedure pointer is in association with
    !>`[[catechin_procedure_logMessage(module):log_debug(subroutine)]]`.
    !>- the `[[catechin_procedure_logMessage(module):log_debug(subroutine)]]`
    !> used with a purpose-specific logger writes
    !> a message prepended `DEBUG: ` to an added log file.
    !>
    subroutine test_log_message_procedure_debug(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        procedure(Ilog_message), pointer :: log_message

        log_message => log_debug

        call check(error, associated(log_message), &
                   message="log_message procedure pointer does not associate with `log_debug`")
        if (occurred(error)) return

        ! calling log_debug in trace logger
        call test_log_message_procedure(log_message, Lv%DEBUG, Pur%Trace, "trace", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_debug in report logger
        call test_log_message_procedure(log_message, Lv%DEBUG, Pur%Report, "report", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_debug in develop logger
        call test_log_message_procedure(log_message, Lv%DEBUG, Pur%Develop, "develop", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_debug in monitor logger
        call test_log_message_procedure(log_message, Lv%DEBUG, Pur%Monitor, "monitor", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        log_message => null()
    end subroutine test_log_message_procedure_debug

    !>testing the procedure `[[catechin_procedure_logMessage(module):log_info(subroutine)]]`
    !>with associating with a procedure pointer.
    !>
    !>This test is checking
    !>
    !>- a procedure pointer is in association with
    !>`[[catechin_procedure_logMessage(module):log_info(subroutine)]]`.
    !>- the `[[catechin_procedure_logMessage(module):log_info(subroutine)]]`
    !> used with a purpose-specific logger writes
    !> a message prepended `INFO: ` to an added log file.
    !>
    subroutine test_log_message_procedure_info(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        procedure(Ilog_message), pointer :: log_message

        log_message => log_info

        call check(error, associated(log_message), &
                   message="log_message procedure pointer does not associate with `log_info`")
        if (occurred(error)) return

        ! calling log_info in trace logger
        call test_log_message_procedure(log_message, Lv%INFO, Pur%Trace, "trace", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_info in report logger
        call test_log_message_procedure(log_message, Lv%INFO, Pur%Report, "report", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_info in develop logger
        call test_log_message_procedure(log_message, Lv%INFO, Pur%Develop, "develop", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_info in monitor logger
        call test_log_message_procedure(log_message, Lv%INFO, Pur%Monitor, "monitor", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        log_message => null()
    end subroutine test_log_message_procedure_info

    !>testing the procedure `[[catechin_procedure_logMessage(module):log_warn(subroutine)]]`
    !>with associating with a procedure pointer.
    !>
    !>This test is checking
    !>
    !>- a procedure pointer is in association with
    !>`[[catechin_procedure_logMessage(module):log_warn(subroutine)]]`.
    !>- the `[[catechin_procedure_logMessage(module):log_warn(subroutine)]]`
    !> used with a purpose-specific logger writes
    !> a message prepended `WARN: ` to an added log file.
    !>
    subroutine test_log_message_procedure_warn(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        procedure(Ilog_message), pointer :: log_message

        log_message => log_warn

        call check(error, associated(log_message), &
                   message="log_message procedure pointer does not associate with `log_warn`")
        if (occurred(error)) return

        ! calling log_warn in trace logger
        call test_log_message_procedure(log_message, Lv%WARN, Pur%Trace, "trace", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_warn in report logger
        call test_log_message_procedure(log_message, Lv%WARN, Pur%Report, "report", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_warn in develop logger
        call test_log_message_procedure(log_message, Lv%WARN, Pur%Develop, "develop", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_warn in monitor logger
        call test_log_message_procedure(log_message, Lv%WARN, Pur%Monitor, "monitor", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        log_message => null()
    end subroutine test_log_message_procedure_warn

    !>testing the procedure `[[catechin_procedure_logMessage(module):log_error(subroutine)]]`
    !>with associating with a procedure pointer.
    !>
    !>This test is checking
    !>
    !>- a procedure pointer is in association with
    !>`[[catechin_procedure_logMessage(module):log_error(subroutine)]]`.
    !>- the `[[catechin_procedure_logMessage(module):log_error(subroutine)]]`
    !> used with a purpose-specific logger writes
    !> a message prepended `ERROR: ` to an added log file.
    !>
    subroutine test_log_message_procedure_error(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        procedure(Ilog_message), pointer :: log_message

        log_message => log_error

        call check(error, associated(log_message), &
                   message="log_message procedure pointer does not associate with `log_error`")
        if (occurred(error)) return

        ! calling log_error in trace logger
        call test_log_message_procedure(log_message, Lv%ERROR, Pur%Trace, "trace", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_error in report logger
        call test_log_message_procedure(log_message, Lv%ERROR, Pur%Report, "report", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_error in develop logger
        call test_log_message_procedure(log_message, Lv%ERROR, Pur%Develop, "develop", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        ! calling log_error in monitor logger
        call test_log_message_procedure(log_message, Lv%ERROR, Pur%Monitor, "monitor", error)
        if (occurred(error)) then
            log_message => null()
            return
        end if

        log_message => null()
    end subroutine test_log_message_procedure_error

    !>Write a log message to a file and read the message from the file.
    !>
    subroutine test_log_message_procedure(log_message, level, purpose, logger_name, error)
        use :: stdlib_ascii
        implicit none
        procedure(Ilog_message), pointer :: log_message
        type(log_level_enum_type), intent(in) :: level
        type(log_purpose_enum_type), intent(in) :: purpose
        character(*), intent(in) :: logger_name
        type(error_type), allocatable, intent(out) :: error

        type(stdlib_logger_type), pointer :: logger
        character(:), allocatable :: log_filename, log_msg, line
        integer(int32) :: log_unit
        integer(int32) :: range(2)

        ! status
        integer(int32) :: stat
        character(:), allocatable :: msg

        !|#### test procedure
        !1. select a specific-purpose logger
        logger => logger_selector(purpose)

        !!1. deactivete writing timestamp and set threshold to debug level
        call logger%configure(time_stamp=.false., level=debug_level)

        !!1. set log filename
        log_filename = logger_name//".log"

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
        !!ex)<br> `DEBUG: output by trace logger`
        log_msg = "output by "//logger_name//" logger"
        call log_message(logger, message=log_msg)

        !!1. remove the log file unit with closeing the unit
        call logger%remove_log_unit(log_unit, close_unit=.true.)

        !!1. open the log file
        open (newunit=log_unit, file=log_filename, action="read", position="rewind", iostat=stat)
        call check(error, stat == success, &
                   message="Could not open the log file "//log_filename)
        if (occurred(error)) return

        !!1. read log message written in the log file
        allocate_str: block
            integer(int32) :: length
            length = len(level%as_string()//": ") + len(log_msg)

            allocate (character(length) :: line)
            allocate (character(256) :: msg)
        end block allocate_str

        read (log_unit, '(A)', iostat=stat, iomsg=msg) line
        call check(error, stat, success, &
                   message="Could not read the log file "//log_filename &
                   //" with error message "//trim(msg))
        if (occurred(error)) return

        !!1. compare the message prefix<br>
        !!ex)<br>
        !!`.123456`<br>
        !!`"DEBUG: output by trace logger" == to_upper("debug")`<br>
        !!`.^---^`
        range(1) = 1
        range(2) = len(level%as_string())
        call check(error, line(range(1):range(2)), to_upper(level%as_string()), &
                   message="written log prefix and read log prefix are different")
        if (occurred(error)) return

        !!1. compare the log messages<br>
        !!ex)<br>
        !!`.12345678901234567890123456789`<br>
        !!`"DEBUG: output by trace logger" == "output by trace logger"`<br>
        !!`........^--------------------^`
        range(1) = range(2) + len(": ") + 1
        range(2) = range(1) - 1 + len(log_msg)
        call check(error, line(range(1):range(2)), log_msg, &
                   message="written message and read massage are different")
        if (occurred(error)) return

        !!1. close and delete the log file
        close (log_unit, status="delete", iostat=stat)
        call check(error, stat == success, &
                   message="Could not delete the log file "//log_filename)
        if (occurred(error)) return

        deallocate (log_filename)
        deallocate (line)
        deallocate (msg)
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
                    close (unit, status="delete", iostat=stat)
                end if
            else
                stat = success
            end if
        end subroutine delete_existing_log_file
    end subroutine test_log_message_procedure
end module test_mod_logMessage

program test_logMessage
    use, intrinsic :: iso_fortran_env
    use :: test_mod_logMessage
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("catechin_procedure % logMessage", collect) &
                  ]
    call run_test(test_suites)
end program test_logMessage
