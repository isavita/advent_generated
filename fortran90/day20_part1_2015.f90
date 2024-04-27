program presents
    implicit none
    integer, parameter :: max_houses = 1000000
    integer :: target, houses(max_houses), i, j
    open(unit=10, file='input.txt', status='old', action='read')
    read(10, *) target
    target = target / 10
    houses = 0
    do i = 1, target
        do j = i, min(target, max_houses), i
            houses(j) = houses(j) + i
        end do
    end do
    do i = 1, min(target, max_houses)
        if (houses(i) >= target) then
            write (*, *) i
            stop
        end if
    end do
end program presents