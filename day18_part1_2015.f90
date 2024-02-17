
program day18
    character(len=100) :: grid(100)
    character(len=100) :: next_grid(100)
    integer :: i, j, step

    open(unit=10, file='input.txt', status='old', action='read')

    do i = 1, 100
        read(10, '(A100)') grid(i)
    end do

    close(10)

    do step = 1, 100
        do i = 1, 100
            do j = 1, 100
                call update_light(i, j, grid, next_grid)
            end do
        end do

        grid = next_grid
    end do

    print *, count_lights_on(grid)

contains

    subroutine update_light(i, j, grid, next_grid)
        character(len=100), intent(in) :: grid(:)
        character(len=100), intent(out) :: next_grid(:)
        integer, intent(in) :: i, j
        integer :: count, x, y

        count = 0

        do x = -1, 1
            do y = -1, 1
                if (x == 0 .and. y == 0) cycle

                if (i + x >= 1 .and. i + x <= 100 .and. j + y >= 1 .and. j + y <= 100) then
                    if (grid(i + x)(j + y:j + y) == '#') count = count + 1
                end if
            end do
        end do

        if (grid(i)(j:j) == '#' .and. (count == 2 .or. count == 3)) then
            next_grid(i)(j:j) = '#'
        elseif (grid(i)(j:j) == '.' .and. count == 3) then
            next_grid(i)(j:j) = '#'
        else
            next_grid(i)(j:j) = '.'
        end if
    end subroutine update_light

    function count_lights_on(grid) result(num_lights_on)
        character(len=100), intent(in) :: grid(:)
        integer :: num_lights_on, i, j

        num_lights_on = 0

        do i = 1, 100
            do j = 1, 100
                if (grid(i)(j:j) == '#') num_lights_on = num_lights_on + 1
            end do
        end do
    end function count_lights_on

end program day18
