module test_mod_userDefinedLogger
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, only:logger_type
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
                     ]
    end subroutine collect

    !>testing the procedure `[[catechin__logger_selector]]` with the argument `purpose=Purpose_Trace`.
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

        logger => catechin__logger_selector(Purpose_Trace)

        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(trace)), &
                   message="the address of selected logger is not the same it of defined trace logger")
        if (occurred(error)) return
    end subroutine test_logger_selector_trace

    !>testing the procedure `[[catechin__logger_selector]]` with the argument `purpose=Purpose_Report`.
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

        logger => catechin__logger_selector(Purpose_Report)
        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(report)), &
                   message="the address of selected logger is not the same it of defined report logger")
        if (occurred(error)) return
    end subroutine test_logger_selector_report

    !>testing the procedure `[[catechin__logger_selector]]` with the argument `purpose=Purpose_Develop`.
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

        logger => catechin__logger_selector(Purpose_Develop)
        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(develop)), &
                   message="the address of selected logger is not the same it of defined develop logger")
        if (occurred(error)) return
    end subroutine test_logger_selector_develop

    !>testing the procedure `[[catechin__logger_selector]]` with the argument `purpose=Purpose_Measure`.
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

        logger => catechin__logger_selector(Purpose_Measure)
        call check(error, associated(logger), &
                   message="logger is not associated")
        if (occurred(error)) return

        call check(error, c_associated(c_loc(logger), c_loc(measure)), &
                   message="the address of selected logger is not the same it of defined measure logger")
        if (occurred(error)) return
    end subroutine test_logger_selector_measure

    !>testing the procedure `[[catechin__logger_selector]]` with an unexpected argument.
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

                logger => catechin__logger_selector(purpose_id)
                call check(error,.not. associated(logger), &
                           message="logger is associated unexpectedly")
                if (occurred(error)) exit

            end if
            purpose_id = -purpose_id/2

        end do
    end subroutine test_logger_selector_null
end module test_mod_userDefinedLogger

program test_userDefinedLogger
    use, intrinsic :: iso_fortran_env
    use :: test_mod_userDefinedLogger
    use :: testdrive, only:run_testsuite, new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("catechin_userDefinedLogger % catechin__logger_selector", collect) &
                  ]
    call run_test(test_suites)
end program test_userDefinedLogger
