module catechin_type_enum_logPurpose
    use :: enumul
    implicit none
    private

    type, public, extends(enum_atype) :: log_purpose_enum_type
        character(64), public :: name = ""
    contains
        procedure, public, pass :: as_string
    end type log_purpose_enum_type

contains
    function as_string(this) result(str)
        implicit none
        class(log_purpose_enum_type), intent(in) :: this
        character(:), allocatable :: str

        str = trim(this%name)
    end function as_string
end module catechin_type_enum_logPurpose
