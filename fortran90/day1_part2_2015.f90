program EnterBasement
    implicit none
    character(1) :: ch
    integer :: floor, position
    integer :: ios  ! Variable to hold the I/O status

    open(unit=10, file='input.txt', status='old')

    floor = 0
    position = 0  ! Initialize position counter
    do
        read(10, '(A1)', advance='no', iostat=ios) ch
        position = position + 1  ! Increment position for each character read
        if (ios /= 0) exit  ! Exit the loop on end of file or error

        select case (ch)
        case ('(')
            floor = floor + 1
        case (')')
            floor = floor - 1
        case default
            ! Ignore any other characters
        end select

        if (floor == -1) then
            print *, position
            exit  ! Exit the loop as soon as Santa reaches the basement
        endif
    end do
    close(10)
end program EnterBasement

