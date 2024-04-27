program rope_bridge
    implicit none
    integer, parameter :: dx = 1000
    integer :: head(2) = 0, tail(2) = 0, i, num_steps
    character(len=1) :: dir
    logical, dimension(dx, dx) :: visited
    integer :: unit = 10

    open(unit, file='input.txt', status='old', action='read')

    visited = .false.
    visited(500, 500) = .true.

    do
        read(unit, *, end=10) dir, num_steps
        do i = 1, num_steps
            select case (dir)
                case ('R')
                    head(1) = head(1) + 1
                case ('L')
                    head(1) = head(1) - 1
                case ('U')
                    head(2) = head(2) + 1
                case ('D')
                    head(2) = head(2) - 1
            end select

            if (abs(head(1) - tail(1)) > 1 .or. abs(head(2) - tail(2)) > 1) then
                if (head(1) /= tail(1) .and. head(2) /= tail(2)) then
                    if (head(1) > tail(1)) then
                        tail(1) = tail(1) + 1
                    else
                        tail(1) = tail(1) - 1
                    end if
                    if (head(2) > tail(2)) then
                        tail(2) = tail(2) + 1
                    else
                        tail(2) = tail(2) - 1
                    end if
                else
                    if (head(1) > tail(1)) then
                        tail(1) = tail(1) + 1
                    else if (head(1) < tail(1)) then
                        tail(1) = tail(1) - 1
                    end if
                    if (head(2) > tail(2)) then
                        tail(2) = tail(2) + 1
                    else if (head(2) < tail(2)) then
                        tail(2) = tail(2) - 1
                    end if
                end if
            end if

            visited(tail(1) + 500, tail(2) + 500) = .true.
        end do
    end do

10  continue
    print *, count(visited)
end program rope_bridge