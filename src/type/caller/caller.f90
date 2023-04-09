module catechin_type_caller
    implicit none
    private
    public :: caller

    type, public :: caller_type
        character(:), allocatable :: module
        character(:), allocatable :: procedure
    end type caller_type

    interface caller
        procedure :: contsruct_caller_type
    end interface

contains
    function contsruct_caller_type(module, procedure) result(new_caller)
        implicit none
        character(*), intent(in) :: module
        character(*), intent(in) :: procedure

        type(caller_type) :: new_caller

        new_caller%module = module
        new_caller%procedure = procedure
    end function contsruct_caller_type
end module catechin_type_caller
