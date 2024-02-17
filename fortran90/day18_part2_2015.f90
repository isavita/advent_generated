
program solution
    implicit none
    integer, parameter :: gridSize = 100
    integer, parameter :: steps = 100
    logical :: grid(gridSize, gridSize)
    integer :: x, y, dx, dy, onNeighbors, onCount, i
    character(len=100) :: line
    integer :: fileUnit
    open(unit=fileUnit, file='input.txt', status='old')
    
    do y = 1, gridSize
        read(fileUnit, '(A)') line
        do x = 1, len_trim(line)
            grid(x, y) = line(x:x) == '#'
        end do
    end do
    
    ! Initialize corners as always on
    grid(1, 1) = .true.
    grid(1, gridSize) = .true.
    grid(gridSize, 1) = .true.
    grid(gridSize, gridSize) = .true.
    
    do i = 1, steps
        call step(grid)
    end do
    
    onCount = 0
    do y = 1, gridSize
        do x = 1, gridSize
            if (grid(x, y)) then
                onCount = onCount + 1
            end if
        end do
    end do
    
    print*, onCount
    
contains

    subroutine step(grid)
        logical :: grid(gridSize, gridSize)
        logical :: newGrid(gridSize, gridSize)
        integer :: x, y, dx, dy, onNeighbors
        
        do y = 1, gridSize
            do x = 1, gridSize
                onNeighbors = countOnNeighbors(grid, x, y)
                if (grid(x, y)) then
                    newGrid(x, y) = onNeighbors == 2 .or. onNeighbors == 3
                else
                    newGrid(x, y) = onNeighbors == 3
                end if
            end do
        end do
        
        ! Ensure corners are always on
        newGrid(1, 1) = .true.
        newGrid(1, gridSize) = .true.
        newGrid(gridSize, 1) = .true.
        newGrid(gridSize, gridSize) = .true.
        
        grid = newGrid
    end subroutine step

    integer function countOnNeighbors(grid, x, y)
        logical :: grid(gridSize, gridSize)
        integer :: x, y, dx, dy, count
        count = 0
        do dx = -1, 1
            do dy = -1, 1
                if (dx /= 0 .or. dy /= 0) then
                    if (x+dx >= 1 .and. x+dx <= gridSize .and. y+dy >= 1 .and. y+dy <= gridSize .and. grid(x+dx, y+dy)) then
                        count = count + 1
                    end if
                end if
            end do
        end do
        countOnNeighbors = count
    end function countOnNeighbors

end program solution
