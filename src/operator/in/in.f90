module catechin_operator_in
    use :: catechin_type
    implicit none
    private
    public :: operator(.in.)

    interface operator(.in.)
        procedure :: set_procedure_and_module
        procedure :: set_procedure_name
        procedure :: set_module_name
    end interface

contains
    function set_procedure_and_module(logger, names) result(new_logger)
        implicit none
        !&<
        type(logger_type), intent(in) :: logger
        type(caller_type), intent(in) :: names
        !&>
        type(logger_type) :: new_logger

        new_logger = logger
        call new_logger%set_module_and_procedure(names%module, names%procedure)
    end function set_procedure_and_module

    function set_procedure_name(logger, proc) result(new_logger)
        implicit none
        !&<
        type(logger_type)           , intent(in) :: logger
        type(procedure_name_type)   , intent(in) :: proc
        !&>
        type(logger_type) :: new_logger

        new_logger = logger
        call new_logger%set_procedure_name(proc%name)
    end function set_procedure_name

    function set_module_name(logger, mod) result(new_logger)
        implicit none
        !&<
        type(logger_type)       , intent(in) :: logger
        type(module_name_type)  , intent(in) :: mod
        !&>
        type(logger_type) :: new_logger

        new_logger = logger
        call new_logger%set_module_name(mod%name)
    end function set_module_name
end module catechin_operator_in
