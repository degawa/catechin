module catechin_procedure_configure
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger, stdlib_logger_type => logger_type
    use :: stdlib_optval
    use :: catechin_userDefinedLogger
    use :: catechin_procedure_logMessage
    implicit none
    private
    public :: configure

contains
    subroutine configure(purpose, level, timestamp, filename, stdout, log_unit, stat)
        implicit none
        integer(int32), intent(in) :: purpose
            !! purpose of logging
        character(*), intent(in), optional :: level
            !! severity level for ignoring a log message
        logical, intent(in), optional :: timestamp
            !! flag to prepend the time stamp
        character(*), intent(in), optional :: filename
            !! filename for logging of specific purpose
        logical, intent(in), optional :: stdout
            !! flag to write a log message to stdout when log messages are output to a file
        integer(int32), intent(out), optional :: log_unit
            !! unit number opened to output log messages to a file
        integer(int32), intent(out), optional :: stat
            !! status of configuration

        type(stdlib_logger_type), pointer :: logger
        integer(int32) :: add_log_file_stat, add_log_unit_stat

        logger => logger_selector(purpose)

        set_level: block
            logical :: valid
            integer(int32) :: threshold

            valid = .false.
            if (present(level)) then
                threshold = as_integer(level)
                if (threshold == debug_level .or. &
                    threshold == information_level .or. &
                    threshold == warning_level .or. &
                    threshold == error_level) &
                    valid = .true.
            end if

            if (valid) then
                call logger%configure(level=threshold)
            end if
        end block set_level

        set_timestamp: block
            call logger%configure(time_stamp=optval(timestamp, .true.))
        end block set_timestamp

        set_logfilename: block
            logical :: has_name, displayable

            has_name = .false.
            if (present(filename)) then
                if (len(filename) > len("")) has_name = .true.
            end if

            if (has_name) then
                call logger%add_log_file(filename, unit=log_unit, stat=add_log_file_stat)

                if (add_log_file_stat /= success .and. present(stat)) then
                    stat = add_log_file_stat
                    return
                end if
            end if

            displayable = .false.
            if (present(stdout)) then
                if (stdout) displayable = .true.
            end if

            if ((has_name .and. add_log_file_stat == success) .and. displayable) then
                call logger%add_log_unit(output_unit, stat=add_log_unit_stat)

                if (add_log_unit_stat /= success .and. present(stat)) then
                    stat = add_log_unit_stat
                    return
                end if
            end if
        end block set_logfilename
    end subroutine configure
end module catechin_procedure_configure
