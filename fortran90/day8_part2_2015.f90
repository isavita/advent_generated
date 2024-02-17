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
        total = total + len_trim(encode(code)) - code_len
    end do

    close(99)

    print *, total

contains

    pure function encode(code) result(newcode)
        character(len=*), intent(in) :: code
        character(len=80) :: newcode
        integer :: i, j

        newcode = '"'
        j = 2

        do i = 1, len_trim(code)
            select case (code(i:i))
            case ('\')
                newcode(j:j+1) = '\\'
                j = j + 2
            case ('"')
                newcode(j:j+1) = '\"'
                j = j + 2
            case default
                newcode(j:j) = code(i:i)
                j = j + 1
            end select
        end do

        newcode(j:j) = '"'
    end function encode


end program day08
