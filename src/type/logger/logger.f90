module catechin_type_logger
    use :: stdlib_logger, stdlib_logger_type => logger_type
    use :: catechin_procedure_logMessage, only:Ilog_message
    implicit none
    private

    !>
    type, public :: logger_type
        type(stdlib_logger_type), private, pointer :: logger => null()
            !! stdlib logger
        procedure(Ilog_message), private, pointer, nopass :: log_message => null()
            !! pointer to a log output procedure
        character(:), allocatable, private :: message
            !! log message
        character(:), allocatable, private :: purpose
            !! purpose of log output
        character(:), allocatable, private :: category
            !! log category
        character(:), allocatable, private :: procedure
            !! procedure name calling log output
        character(:), allocatable, private :: module
            !! module name containing the procedure calling log output
    contains
        procedure, public, pass :: logging
        !* output log

        procedure, public, pass :: set_logger
        !* associate logger
        procedure, public, pass :: set_log_message_proc
        !* associate log output procedure
        procedure, public, pass :: set_purpose
        !* set purpose
        procedure, public, pass :: set_category
        !* set category
        procedure, public, pass :: set_message
        !* set log message
        procedure, public, pass :: set_module_and_procedure
        !* set module and procedure name
        procedure, public, pass :: set_module_name
        !* set module name
        procedure, public, pass :: set_procedure_name
        !* set procedure name
    end type logger_type

    interface operator(==)
        procedure :: eqv_logical_logical
    end interface
contains
    subroutine logging(this)
        implicit none
        class(logger_type), intent(in) :: this
        character(:), allocatable :: msg
        logical :: alloced(2)

        if (.not. associated(this%log_message) .and. .not. associated(this%logger)) then
            return
        end if

        msg = ""
        if (allocated(this%purpose)) msg = msg//"["//this%purpose//"]"//": "
        if (allocated(this%category)) msg = msg//"["//this%category//"]"//": "
        if (allocated(this%message)) msg = msg//this%message

        alloced(:) = [allocated(this%module), allocated(this%procedure)]
        if (alloced == [.true., .true.]) then
            call this%log_message(this%logger, msg, this%module, this%procedure)
        else if (alloced == [.true., .false.]) then
            call this%log_message(this%logger, msg, module=this%module)
        else if (alloced == [.false., .true.]) then
            call this%log_message(this%logger, msg, procedure=this%procedure)
        else
            call this%log_message(this%logger, msg)
        end if
    end subroutine logging

    subroutine set_logger(this, logger)
        implicit none
        class(logger_type), intent(inout) :: this
        type(stdlib_logger_type), pointer, intent(in) :: logger

        this%logger => logger
    end subroutine set_logger

    subroutine set_log_message_proc(this, log_message)
        implicit none
        class(logger_type), intent(inout) :: this
        procedure(Ilog_message), pointer, intent(in) :: log_message

        this%log_message => log_message
    end subroutine set_log_message_proc

    subroutine set_purpose(this, purpose)
        implicit none
        class(logger_type), intent(inout) :: this
        character(*), intent(in) :: purpose

        this%purpose = purpose
    end subroutine set_purpose

    subroutine set_category(this, category)
        implicit none
        class(logger_type), intent(inout) :: this
        character(*), intent(in) :: category

        this%category = category
    end subroutine set_category

    subroutine set_message(this, message)
        implicit none
        class(logger_type), intent(inout) :: this
        character(*), intent(in) :: message

        this%message = message
    end subroutine set_message

    subroutine set_module_and_procedure(this, module, procedure)
        implicit none
        class(logger_type), intent(inout) :: this
        character(*), intent(in) :: module
        character(*), intent(in) :: procedure

        call this%set_module_name(module)
        call this%set_procedure_name(procedure)
    end subroutine set_module_and_procedure

    subroutine set_module_name(this, module)
        implicit none
        class(logger_type), intent(inout) :: this
        character(*), intent(in) :: module

        this%module = module
    end subroutine set_module_name

    subroutine set_procedure_name(this, procedure)
        implicit none
        class(logger_type), intent(inout) :: this
        character(*), intent(in) :: procedure

        this%procedure = procedure
    end subroutine set_procedure_name

    pure logical function eqv_logical_logical(lhs, rhs)
        implicit none
        logical, intent(in) :: lhs(:)
            !! the left-hand side of the operator
        logical, intent(in) :: rhs(:)
            !! the right-hand side of the operator

        eqv_logical_logical = all(lhs .eqv. rhs)
    end function eqv_logical_logical
end module catechin_type_logger
