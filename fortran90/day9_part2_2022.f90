program rope_bridge
    implicit none
    integer, parameter :: ropelen = 10
    integer, dimension(ropelen, 2) :: rope
    integer, dimension(2) :: d
    integer :: i, j, n, x, y
    character :: b
    logical, dimension(-2000:2000, -2000:2000) :: visited
    integer :: count

    ! Initialize the rope array
    rope = 0

    open(unit=10, file='input.txt', status='old')
    do
        read(10, *, end=10) b, n
        d = dir_from_byte(b)
        do i = 1, n
            rope(1, 1) = rope(1, 1) + d(1)
            rope(1, 2) = rope(1, 2) + d(2)
            do j = 2, ropelen
                if (abs(rope(j-1, 1) - rope(j, 1)) > 1 .or. abs(rope(j-1, 2) - rope(j, 2)) > 1) then
                    rope(j, 1) = rope(j, 1) + sign(rope(j-1, 1) - rope(j, 1))
                    rope(j, 2) = rope(j, 2) + sign(rope(j-1, 2) - rope(j, 2))
                end if
            end do
            visited(rope(ropelen, 1), rope(ropelen, 2)) = .true.
        end do
    end do
10  close(10)
    count = 0
    do i = -2000, 2000
        do j = -2000, 2000
            if (visited(i, j)) count = count + 1
        end do
    end do
    print *, count
contains
    function dir_from_byte(b) result(d)
        character, intent(in) :: b
        integer, dimension(2) :: d
        select case (b)
            case ('N', 'U', '^')
                d = [0, 1]
            case ('E', 'R', '>')
                d = [1, 0]
            case ('S', 'D', 'v')
                d = [0, -1]
            case ('W', 'L', '<')
                d = [-1, 0]
        end select
    end function dir_from_byte

    function sign(x) result(y)
        integer, intent(in) :: x
        integer :: y
        if (x > 0) then
            y = 1
        else if (x < 0) then
            y = -1
        else
            y = 0
        end if
    end function sign
end program rope_bridge