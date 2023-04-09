module catechin_type_procedure
    implicit none
    private
    public :: procedure

    type, public :: procedure_name_type
        character(:), allocatable :: name
    end type procedure_name_type

    interface procedure
        procedure :: contsruct_procedure_name_type
    end interface

contains
    function contsruct_procedure_name_type(name) result(new_procedure_name)
        implicit none
        character(*), intent(in) :: name

        type(procedure_name_type) :: new_procedure_name

        new_procedure_name%name = name
    end function contsruct_procedure_name_type
end module catechin_type_procedure
