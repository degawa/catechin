module catechin_operator_message
    use :: catechin_type
    implicit none
    private
    public :: operator(.message.)

    interface operator(.message.)
        procedure :: set_message
    end interface

contains
    function set_message(logger, message) result(new_logger)
        implicit none
        !&<
        type(logger_type)   , intent(in) :: logger
        character(*)        , intent(in) :: message
        !&>
        type(logger_type) :: new_logger

        new_logger = logger
        call new_logger%set_message(message)
    end function set_message
end module catechin_operator_message
