program fuel_calculator
    implicit none
    integer, parameter :: max_positions = 1000
    integer :: positions(max_positions), min_fuel, fuel, i, j, n
    open(unit=10, file='input.txt', status='old')
    read(10, *) (positions(i), i=1, max_positions)
    close(10)
    call sort(positions, n)
    min_fuel = huge(min_fuel)
    do i = positions(1), positions(n)
        fuel = 0
        do j = 1, n
            fuel = fuel + calculate_fuel(positions(j), i)
        end do
        if (fuel < min_fuel) then
            min_fuel = fuel
        end if
    end do
    print *, min_fuel
contains
    function calculate_fuel(current_position, new_position) result(fuel)
        integer, intent(in) :: current_position, new_position
        integer :: fuel
        fuel = abs(current_position - new_position)
    end function calculate_fuel

    function abs(n) result(res)
        integer, intent(in) :: n
        integer :: res
        if (n < 0) then
            res = -n
        else
            res = n
        end if
    end function abs

    subroutine sort(a, n)
        integer, intent(inout) :: a(:)
        integer, intent(out) :: n
        integer :: i, temp
        n = size(a)
        do i = 1, n-1
            do j = i+1, n
                if (a(i) > a(j)) then
                    temp = a(i)
                    a(i) = a(j)
                    a(j) = temp
                end if
            end do
        end do
    end subroutine sort
end program fuel_calculator