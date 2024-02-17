program day08
    implicit none

    character(len=80) :: code
    integer :: ios
    integer :: total = 0
    integer :: code_len

    open(99, file='input.txt', action='read', position='rewind')

    do
        read(99, '(A)', iostat=ios) code
        if (ios /= 0) exit
        code_len = len_trim(code)
        total = total + code_len - count_string(code)
    end do

    close(99)

    print *, total

contains

    pure function count_string(code) result(strlen)
        integer :: strlen
        character(len=*), intent(in) :: code
        integer :: i

        i = 2; strlen = 0

        do 
            select case (code(i:i))
            case ('\')
                select case (code(i+1:i+1))
                case ('x')
                    i = i + 3
                case default
                    i = i + 1
                end select
            case ('"')
                exit
            end select
            strlen = strlen + 1
            i = i + 1
        end do
    end function count_string

end program day08
