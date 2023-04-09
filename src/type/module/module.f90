module catechin_type_module
    implicit none
    private
    public :: module

    type, public :: module_name_type
        character(:), allocatable :: name
    end type module_name_type

    interface module
        procedure :: contsruct_module_name_type
    end interface

contains
    function contsruct_module_name_type(name) result(new_module_name)
        implicit none
        character(*), intent(in) :: name

        type(module_name_type) :: new_module_name

        new_module_name%name = name
    end function contsruct_module_name_type
end module catechin_type_module
