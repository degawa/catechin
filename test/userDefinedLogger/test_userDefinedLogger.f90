module test_mod_userDefinedLogger
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:stdlib_logger_type => logger_type, success
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:occurred
    use :: catechin_userDefinedLogger
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
                     new_unittest("The logger selected by `logger_selecter(Pur%Trace)` &
                                  &is the same as the pre-declared logger used for trace.", &
                                  test_logger_selector_trace) &
                     , new_unittest("The logger selected by `logger_selecter(Pur%Report)` &
                                    &is the same as the pre-declared logger used for report", &
                                    test_logger_selector_report) &
                     , new_unittest("The logger selected by `logger_selecter(Pur%Develop) &
                                    &is the same as the pre-declared logger used for development", &
                                    test_logger_selector_develop) &
                     , new_unittest("The logger selected by `logger_selecter(Pur%Monitor) &
                                    &is the same as the pre-declared logger used for monitoring", &
                                    test_logger_selector_monitor) &
                     , new_unittest("The logger returned by `logger_selecter` with an unexpected argument is null", &
                                    test_logger_selector_null) &
                     , new_unittest("The logger returned by `logger_selecter` is able to output log messages", &
                                    test_selected_logger_logging) &
                     ]
    end subroutine collect

    !>test the procedure `[[logger_selector]]` with the argument `purpose=Pur%Trace`.
    !>
    !>This test is checking
    !>
    !>- the returned logger is not null by associating with a pointer and its status.
    !>- the address of returned logger is the same as it of the corresponding logger defined in `[[catechin_userDefinedLogger]]`.
    !>
    subroutine test_logger_selector_trace(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger

        logger => logger_selector(Pur%Trace)

        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(trace)), &
                   message="the address of selected logger is not the same it of defined trace logger")
        if (occurred(error)) then
            logger => null()
            return
        end if
    end subroutine test_logger_selector_trace

    !>test the procedure `[[logger_selector]]` with the argument `purpose=Pur%Report`.
    !>
    !>This test is checking
    !>
    !>- the returned logger is not null by associating with a pointer and its status.
    !>- the address of returned logger is the same as it of the corresponding logger defined in `[[catechin_userDefinedLogger]]`.
    !>
    subroutine test_logger_selector_report(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger

        logger => logger_selector(Pur%Report)
        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(report)), &
                   message="the address of selected logger is not the same it of defined report logger")
        if (occurred(error)) then
            logger => null()
            return
        end if
    end subroutine test_logger_selector_report

    !>test the procedure `[[logger_selector]]` with the argument `purpose=Pur%Develop`.
    !>
    !>This test is checking
    !>
    !>- the returned logger is not null by associating with a pointer and its status.
    !>- the address of returned logger is the same as it of the corresponding logger defined in `[[catechin_userDefinedLogger]]`.
    !>
    subroutine test_logger_selector_develop(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger

        logger => logger_selector(Pur%Develop)
        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(develop)), &
                   message="the address of selected logger is not the same it of defined develop logger")
        if (occurred(error)) then
            logger => null()
            return
        end if
    end subroutine test_logger_selector_develop

    !>test the procedure `[[logger_selector]]` with the argument `purpose=Pur%Monitor`.
    !>
    !>This test is checking
    !>
    !>- the returned logger is not null by associating with a pointer and its status.
    !>- the address of returned logger is the same as it of the corresponding logger defined in `[[catechin_userDefinedLogger]]`.
    !>
    subroutine test_logger_selector_monitor(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger

        logger => logger_selector(Pur%Monitor)
        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(monitor)), &
                   message="the address of selected logger is not the same it of defined monitor logger")
        if (occurred(error)) then
            logger => null()
            return
        end if
    end subroutine test_logger_selector_monitor

    !>test the procedure `[[logger_selector]]` with an unexpected argument.
    !>
    !>This test is checking
    !>
    !>- the returned logger is null by associating with a pointer and its status.
    !>
    subroutine test_logger_selector_null(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(stdlib_logger_type), pointer :: logger
        integer(int32) :: purpose_id
        type(log_purpose_enum_type) :: purpose

        ! test with arguments in (Int32 minimun,  Int32 maximum]
        purpose = log_purpose_enum_type(huge(purpose_id), "")
        purpose_id = huge(purpose_id)
        do while (abs(purpose_id) > 0)

            if (purpose < Pur%Trace  .or. &
                          Pur%Monitor <= purpose) then !&

                logger => logger_selector(purpose)
                call check(error,.not. associated(logger), &
                           message="logger is associated unexpectedly")
                if (occurred(error)) then
                    logger => null()
                    exit
                end if

            end if
            purpose_id = -purpose_id/2

        end do
    end subroutine test_logger_selector_null

    !>test loggers pointers output log masseges.
    !>
    subroutine test_selected_logger_logging(error)
        use :: stdlib_io
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call test_logger(Pur%Trace, "trace"); if (occurred(error)) return
        call test_logger(Pur%Report, "report"); if (occurred(error)) return
        call test_logger(Pur%Develop, "develop"); if (occurred(error)) return
        call test_logger(Pur%Monitor, "monitor"); if (occurred(error)) return

    contains
        !>Write a log message to a file and read the message from the file.
        subroutine test_logger(purpose, logger_name)
            implicit none
            type(log_purpose_enum_type), intent(in) :: purpose
            character(*), intent(in) :: logger_name

            type(stdlib_logger_type), pointer :: logger
            character(:), allocatable :: log_filename, log_msg, line
            integer(int32) :: log_unit

            ! status
            integer(int32) :: stat
            character(:), allocatable :: msg

            ! select a specific-purpose logger
            logger => logger_selector(purpose)
            call logger%configure(time_stamp=.false.)

            ! set log filename
            log_filename = logger_name//".log"

            ! delete log file if it has already existed
            call delete_existing_log_file(log_filename, stat)
            call check(error, stat, success, &
                       message="Could not continue the test due to failing log file handling")
            if (occurred(error)) return

            ! add the log file to the logger
            call logger%add_log_file(filename=log_filename, unit=log_unit, stat=stat)
            call check(error, stat, success, message="Could not add log file")
            if (occurred(error)) return

            ! output log message to the log file and remove the log file unit
            log_msg = "output by "//logger_name//" logger"
            call logger%log_message(message=log_msg)
            call logger%remove_log_unit(log_unit, close_unit=.true.)

            ! open the log file
            open (newunit=log_unit, file=log_filename, action="read", position="rewind", iostat=stat)
            call check(error, stat, success, &
                       message="Could not open the log file "//log_filename)
            if (occurred(error)) return

            ! read log message written in the log file
            allocate (character(len(log_msg)) :: line)
            allocate (character(256) :: msg)

            read (log_unit, '(A)', iostat=stat, iomsg=msg) line
            call check(error, stat, success, &
                       message="Could not read the log file "//log_filename &
                       //" with error message "//trim(msg))
            if (occurred(error)) return

            ! compare log messages
            call check(error, trim(line), log_msg, &
                       message="output message and read massage are different")
            if (occurred(error)) return

            ! close and delete the log file
            close (log_unit, status="delete", iostat=stat)
            call check(error, stat, success, &
                       message="Could not delete the log file "//log_filename)
            if (occurred(error)) return

            deallocate (log_filename)
            deallocate (line)
            deallocate (msg)
            logger => null()
        end subroutine test_logger

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
    end subroutine test_selected_logger_logging
end module test_mod_userDefinedLogger

program test_userDefinedLogger
    use, intrinsic :: iso_fortran_env
    use :: test_mod_userDefinedLogger
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("catechin_userDefinedLogger % logger_selector", collect) &
                  ]
    call run_test(test_suites)
end program test_userDefinedLogger
