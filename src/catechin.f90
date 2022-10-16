module catechin
    use :: catechin_procedure
    use :: catechin_userDefinedLogger
    implicit none
    private
    public :: catechin__logging
    public :: Lv_DEBUG, Lv_INFO, Lv_WARN, Lv_ERROR
    public :: Purpose_Trace, Purpose_Report, Purpose_Develop, Purpose_Measure

    interface catechin__logging
        procedure :: logging_w_args
    end interface

contains
    !>Write a log massage with prefixes
    !>including log level, purpose, and category.
    !>
    !>This procedure is given a generic name `catechin__logging`
    !>using an interface.
    !>
    !>#### Example
    !>```Fortran
    !>program main
    !>    use :: catechin
    !>    implicit none
    !>
    !>    call catechin__logging(Lv_INFO, "vtr_file%initialize", purpose=Purpose_Trace, category="IO.vector3d.vtr")
    !>    ! YYYY-MM-DD hh:mm:ss.sss: INFO: [trace]: [IO.vector3d.vtr]: vtr_file%initialize
    !>end program main
    !>```
    !>
    subroutine logging_w_args(level, message, module, procedure, purpose, category)
        use, intrinsic :: iso_fortran_env
        implicit none
        !&<
        character(*)    , intent(in)            :: level
            !! log level
        character(*)    , intent(in)            :: message
            !! log message
        integer(int32)  , intent(in), optional  :: purpose
            !! purpose of logging
        character(*)    , intent(in), optional  :: category
            !! log category
        character(*)    , intent(in), optional  :: module
            !! a name of the module containing
            !! the current invocation of this procedure
        character(*)    , intent(in), optional  :: procedure
            !! a name of the procedure containing
            !! the current invocation of this procedure
        !&>
        character(:), allocatable :: prefix
            !! prefix containing level, purpose, and category
        procedure(Ilog_message), pointer :: log_message
            !! a pointer to logging procedure

        ! select log messaging procedure
        log_message => log_message_procedure_selector(level)
        if (.not. associated(log_message)) return

        ! constructing the prefix
        prefix = ""
        if (present(purpose)) prefix = prefix//"["//get_purpose_in_string(purpose)//"]"//": "
        if (present(category)) prefix = prefix//"["//category//"]"//": "

        ! write log massage with specifing purpose-specific logger, messages with prefix,
        ! module name, and purocedure name.
        call log_message(logger_selector(purpose), prefix//message, module, procedure)
    end subroutine logging_w_args

    !>Returns log messaging procedure based on the argument `level`.
    !>
    !>Returns `null` when passed an unexpected actual argument `level`
    !>
    !>@warning
    !>This function must be placed in the same module
    !>as procedures, `[[logging_w_args]]`,
    !>referring to it to avoid compile errors in gfortran and NAG Fortran.
    !>
    !>Intel Fortran compiles successfully even if this function
    !>is in a different module.
    !>Suppose other compilers can compile successfully in the future,
    !>I will move this function to `[[catechin_procedure_logMessage]]`,
    !> and will change the log messaging functions referred to
    !> in this function to private.
    !>@endwarning
    !>
    function log_message_procedure_selector(level) result(log_message)
        use :: catechin_procedure_logMessage
        implicit none
        character(*), intent(in) :: level
        procedure(Ilog_message), pointer :: log_message

        select case (level)
        case (Lv_DEBUG)
            log_message => log_debug

        case (Lv_INFO)
            log_message => log_info

        case (Lv_WARN)
            log_message => log_warn

        case (Lv_ERROR)
            log_message => log_error

        case default
            log_message => null()
        end select
    end function log_message_procedure_selector
end module catechin
