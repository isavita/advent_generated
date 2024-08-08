program sonar_sweep
    implicit none
    integer, allocatable :: depths(:)
    integer :: i, n, count_increases, count_window_increases
    integer :: window_sum1, window_sum2

    ! Read data from input file
    open(unit=10, file='input.txt', status='old', action='read')
    n = 0
    do while (.true.)
        read(10, *, iostat=i)
        if (i /= 0) exit
        n = n + 1
    end do
    rewind(10)  ! Rewind the file to read data again
    allocate(depths(n))

    ! Read the depths into the allocated array
    do i = 1, n
        read(10, *) depths(i)
    end do
    close(10)

    ! Count increases in depth measurements
    count_increases = 0
    do i = 2, n
        if (depths(i) > depths(i-1)) count_increases = count_increases + 1
    end do

    ! Count increases in three-measurement sliding window sums
    count_window_increases = 0
    do i = 4, n
        window_sum1 = depths(i-3) + depths(i-2) + depths(i-1)
        window_sum2 = depths(i-2) + depths(i-1) + depths(i)
        if (window_sum2 > window_sum1) count_window_increases = count_window_increases + 1
    end do

    ! Output results
    print *, 'Depth increases: ', count_increases
    print *, 'Window sum increases: ', count_window_increases

    ! Deallocate memory
    deallocate(depths)
end program sonar_sweep