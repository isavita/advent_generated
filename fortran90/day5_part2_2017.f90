program maze_of_twisty_trampolines
    implicit none
    integer, allocatable :: offsets(:)
    integer :: i, steps, n, jump
    character(len=100) :: line
    integer :: unit

    ! Open the input file
    unit = 10
    open(unit, file='input.txt', status='old', action='read')

    ! Read the offsets from the file
    n = 0
    do while (.true.)
        read(unit, '(A)', iostat=i) line
        if (i /= 0) exit
        n = n + 1
    end do
    rewind(unit)

    allocate(offsets(n))
    do i = 1, n
        read(unit, *) offsets(i)
    end do
    close(unit)

    ! Process the jumps
    i = 1
    steps = 0
    do while (i > 0 .and. i <= n)
        jump = offsets(i)
        offsets(i) = offsets(i) + 1
        if (jump >= 3) then
            offsets(i) = offsets(i) - 2
        end if
        i = i + jump
        steps = steps + 1
    end do

    print *, steps
end program maze_of_twisty_trampolines