program main
    implicit none
    character, parameter :: Open = '.', Trees = '|', Lumberyard = '#'
    integer, parameter :: Size = 50
    character, dimension(Size, Size) :: grid
    integer :: i, j, minute, wooded, lumberyards
    open(unit=10, file='input.txt', status='old', action='read')
    do i = 1, Size
        read(10, '(50A1)') (grid(i, j), j = 1, Size)
    end do
    close(10)
    do minute = 1, 10
        grid = transform(grid)
    end do
    call count_resources(grid, wooded, lumberyards)
    print *, wooded * lumberyards
contains
    function transform(grid) result(newGrid)
        character, dimension(Size, Size), intent(in) :: grid
        character, dimension(Size, Size) :: newGrid
        integer :: i, j
        do i = 1, Size
            do j = 1, Size
                newGrid(i, j) = next_acre_state(grid, i, j)
            end do
        end do
    end function transform

    function next_acre_state(grid, i, j) result(state)
        character, dimension(Size, Size), intent(in) :: grid
        integer, intent(in) :: i, j
        character :: state
        select case (grid(i, j))
            case (Open)
                if (count_adjacent(grid, i, j, Trees) >= 3) then
                    state = Trees
                else
                    state = Open
                end if
            case (Trees)
                if (count_adjacent(grid, i, j, Lumberyard) >= 3) then
                    state = Lumberyard
                else
                    state = Trees
                end if
            case (Lumberyard)
                if (count_adjacent(grid, i, j, Lumberyard) >= 1 .and. count_adjacent(grid, i, j, Trees) >= 1) then
                    state = Lumberyard
                else
                    state = Open
                end if
        end select
    end function next_acre_state

    function count_adjacent(grid, i, j, acreType) result(count)
        character, dimension(Size, Size), intent(in) :: grid
        integer, intent(in) :: i, j
        character, intent(in) :: acreType
        integer :: count, x, y
        count = 0
        do x = -1, 1
            do y = -1, 1
                if (x == 0 .and. y == 0) cycle
                if (i+x >= 1 .and. i+x <= Size .and. j+y >= 1 .and. j+y <= Size .and. grid(i+x, j+y) == acreType) then
                    count = count + 1
                end if
            end do
        end do
    end function count_adjacent

    subroutine count_resources(grid, wooded, lumberyards)
        character, dimension(Size, Size), intent(in) :: grid
        integer, intent(out) :: wooded, lumberyards
        integer :: i, j
        wooded = 0
        lumberyards = 0
        do i = 1, Size
            do j = 1, Size
                select case (grid(i, j))
                    case (Trees)
                        wooded = wooded + 1
                    case (Lumberyard)
                        lumberyards = lumberyards + 1
                end select
            end do
        end do
    end subroutine count_resources
end program main