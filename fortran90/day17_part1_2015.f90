program combinations
    implicit none
    integer, parameter :: max_containers = 100
    integer :: containers(max_containers)
    integer :: n, target, i
    character(len=10) :: buffer

    open(unit=10, file="input.txt", status="old", action="read")
    n = 0
    do
        read(10, *, end=10) buffer
        read(buffer, *) containers(n+1)
        n = n + 1
    end do
    10 close(10)

    target = 150
    write(*, *) count_combinations(containers, target, 0, n)

contains
    recursive function count_combinations(containers, target, index, n) result(res)
        implicit none
        integer, intent(in) :: containers(:), target, index, n
        integer :: res
        if (target == 0) then
            res = 1
        else if (target < 0 .or. index >= n) then
            res = 0
        else
            res = count_combinations(containers, target-containers(index+1), index+1, n) + &
                   count_combinations(containers, target, index+1, n)
        end if
    end function count_combinations
end program combinations