module test_mod_userDefinedLogger
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:logger_type, success
    use :: testdrive, only:new_unittest, unittest_type, error_type, check
    use :: testdrive_util, only:occurred
    use :: catechin_userDefinedLogger
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
                     new_unittest("The logger selected by `logger_selecter(Purpose_Trace)` &
                                  &is the same as the pre-declared logger used for trace.", &
                                  test_logger_selector_trace) &
                     , new_unittest("The logger selected by `logger_selecter(Purpose_Report)` &
                                    &is the same as the pre-declared logger used for report", &
                                    test_logger_selector_report) &
                     , new_unittest("The logger selected by `logger_selecter(Purpose_Develop) &
                                    &is the same as the pre-declared logger used for development", &
                                    test_logger_selector_develop) &
                     , new_unittest("The logger selected by `logger_selecter(Purpose_Measure) &
                                    &is the same as the pre-declared logger used for measurement", &
                                    test_logger_selector_measure) &
                     , new_unittest("The logger returned by `logger_selecter` with an unexpected argument is null", &
                                    test_logger_selector_null) &
                     , new_unittest("The logger returned by `logger_selecter` is able to output log messages", &
                                    test_selected_logger_logging) &
                     ]
    end subroutine collect

    !>testing the procedure `[[logger_selector]]` with the argument `purpose=Purpose_Trace`.
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

        type(logger_type), pointer :: logger

        logger => logger_selector(Purpose_Trace)

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

    !>testing the procedure `[[logger_selector]]` with the argument `purpose=Purpose_Report`.
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

        type(logger_type), pointer :: logger

        logger => logger_selector(Purpose_Report)
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

    !>testing the procedure `[[logger_selector]]` with the argument `purpose=Purpose_Develop`.
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

        type(logger_type), pointer :: logger

        logger => logger_selector(Purpose_Develop)
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

    !>testing the procedure `[[logger_selector]]` with the argument `purpose=Purpose_Measure`.
    !>
    !>This test is checking
    !>
    !>- the returned logger is not null by associating with a pointer and its status.
    !>- the address of returned logger is the same as it of the corresponding logger defined in `[[catechin_userDefinedLogger]]`.
    !>
    subroutine test_logger_selector_measure(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(logger_type), pointer :: logger

        logger => logger_selector(Purpose_Measure)
        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(measure)), &
                   message="the address of selected logger is not the same it of defined measure logger")
        if (occurred(error)) then
            logger => null()
            return
        end if
    end subroutine test_logger_selector_measure

    !>testing the procedure `[[logger_selector]]` with an unexpected argument.
    !>
    !>This test is checking
    !>
    !>- the returned logger is null by associating with a pointer and its status.
    !>
    subroutine test_logger_selector_null(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(logger_type), pointer :: logger
        integer(int32) :: purpose_id

        ! test with arguments in (Int32 minimun,  Int32 maximum]
        purpose_id = huge(purpose_id)
        do while (abs(purpose_id) > 0)

            if (purpose_id < Purpose_Trace  .or. &
                             Purpose_Sentinel <= purpose_id) then !&

                logger => logger_selector(purpose_id)
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

    !>testing loggers pointers output log masseges.
    !>
    subroutine test_selected_logger_logging(error)
        use :: stdlib_io
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call test_logger(Purpose_Trace, "trace"); if (occurred(error)) return
        call test_logger(Purpose_Report, "report"); if (occurred(error)) return
        call test_logger(Purpose_Develop, "develop"); if (occurred(error)) return
        call test_logger(Purpose_Measure, "measure"); if (occurred(error)) return

    contains
        subroutine test_logger(purpose, logger_name)
            implicit none
            integer(int32), intent(in) :: purpose
            character(*), intent(in) :: logger_name

            type(logger_type), pointer :: logger
            character(:), allocatable :: log_filename, log_msg, line
            integer(int32) :: log_unit
            logical :: exists

            ! status
            integer(int32) :: stat
            character(:), allocatable :: msg

            ! select a specific-purpose logger
            logger => logger_selector(purpose)
            call logger%configure(time_stamp=.false.)

            ! set filename and delete it if it has already existed
            log_filename = logger_name//".log"

            inquire (file=log_filename, exist=exists)
            if (exists) then
                delete_log: block
                    open (newunit=log_unit, file=log_filename, status="old", iostat=stat)

                    if (stat == success) then
                        close (log_unit, status="delete", iostat=stat)
                    end if
                    call check(error, stat == success, &
                               message="Could not continue the test due to failing log file handling")
                    if (occurred(error)) return
                end block delete_log
            end if

            ! add the log file to the logger
            call logger%add_log_file(filename=log_filename, unit=log_unit, stat=stat)
            call check(error, stat == success, message="Could not add log file")
            if (occurred(error)) return

            ! output log message to the log file and remove the log file unit
            log_msg = "output by "//logger_name//" logger"
            call logger%log_message(message=log_msg)
            call logger%remove_log_unit(log_unit, close_unit=.true.)

            ! open the log file
            open (newunit=log_unit, file=log_filename, action="read", position="rewind", iostat=stat)
            call check(error, stat == success, &
                       message="Could not open the log file "//log_filename)
            if (occurred(error)) return

            ! read log message written in the log file
            allocate (character(len(log_msg)) :: line)
            allocate (character(256) :: msg)

            read (log_unit, '(A)', iostat=stat, iomsg=msg) line
            call check(error, stat == success, &
                       message="Could not read the log file "//log_filename &
                       //" with error message "//trim(msg))
            if (occurred(error)) return

            ! compare log messages
            call check(error, trim(line) == log_msg, &
                       message="output message and read massage are different")
            if (occurred(error)) return

            ! close and delete the log file
            close (log_unit, status="delete", iostat=stat)
            call check(error, stat == success, &
                       message="Could not delete the log file "//log_filename)
            if (occurred(error)) return

            deallocate (log_filename)
            deallocate (line)
            deallocate (msg)
            logger => null()
        end subroutine test_logger
    end subroutine test_selected_logger_logging
end module test_mod_userDefinedLogger

program test_userDefinedLogger
    use, intrinsic :: iso_fortran_env
    use :: test_mod_userDefinedLogger
    use :: testdrive, only:run_testsuite, new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("catechin_userDefinedLogger % logger_selector", collect) &
                  ]
    call run_test(test_suites)
end program test_userDefinedLogger
