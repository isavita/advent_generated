program NotQuiteLisp
    implicit none
    character(1) :: ch
    integer :: floor
    integer :: ios  ! Variable to hold the I/O status

    open(unit=10, file='input.txt', status='old')

    floor = 0
    do
        read(10, '(A1)', advance='no', iostat=ios) ch
        if (ios /= 0) exit  ! Exit the loop on end of file or error
        select case (ch)
        case ('(')
            floor = floor + 1
        case (')')
            floor = floor - 1
        case default
            ! Ignore any other characters
        end select
    end do
    close(10)

    print *, floor
end program NotQuiteLisp

