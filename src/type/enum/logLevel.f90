module catechin_type_enum_logLevel
    use :: enumul
    implicit none
    private

    type, public, extends(enum_atype) :: log_level_enum_type
        character(64), public :: name = ""
    contains
        procedure, public, pass :: as_integer
        procedure, public, pass :: as_string
    end type log_level_enum_type

contains
    function as_integer(this) result(id)
        use, intrinsic :: iso_fortran_env
        implicit none
        class(log_level_enum_type), intent(in) :: this
        integer(int32) :: id

        id = this%enum
    end function as_integer

    function as_string(this) result(str)
        implicit none
        class(log_level_enum_type), intent(in) :: this
        character(:), allocatable :: str

        str = trim(this%name)
    end function as_string
end module catechin_type_enum_logLevel
