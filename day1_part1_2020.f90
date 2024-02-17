
program solution
    implicit none
    integer :: n, i, j
    integer :: ierr
    integer, dimension(1000) :: numbers

    open(unit=10, file="input.txt", status="old")
    n = 0
    do
        read(10, *, iostat=ierr) numbers(n+1)
        if (ierr /= 0) then
            if (ierr < 0) exit
        end if
        n = n + 1
    end do

    do i = 1, n-1
        do j = i + 1, n
            if (numbers(i) + numbers(j) == 2020) then
                print *, numbers(i) * numbers(j)
                stop
            end if
        end do
    end do

    close(10)
end program solution
