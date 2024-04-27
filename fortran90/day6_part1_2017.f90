program day6
    implicit none
    integer, parameter :: num_banks = 16
    integer :: banks(num_banks), max_blocks, max_index, i, j, cycle_count
    logical :: seen_before
    character(len=100) :: line
    integer, allocatable :: seen_configs(:,:)

    ! Read input from file
    open(unit=10, file='input.txt', status='old')
    read(10, '(A)') line
    close(10)
    read(line, *) banks

    ! Initialize variables
    cycle_count = 0
    seen_before = .false.
    allocate(seen_configs(num_banks, 10000))

    ! Main loop
    do while (.not. seen_before)
        ! Find bank with most blocks
        max_blocks = maxval(banks)
        max_index = minloc(banks, dim=1, mask=banks == max_blocks)

        ! Redistribute blocks
        banks(max_index) = 0
        do i = 1, max_blocks
            max_index = mod(max_index, num_banks) + 1
            banks(max_index) = banks(max_index) + 1
        end do

        ! Check if configuration has been seen before
        do i = 1, size(seen_configs, 2)
            if (all(seen_configs(:, i) == banks)) then
                seen_before = .true.
                exit
            end if
        end do

        ! Store current configuration
        seen_configs(:, cycle_count + 1) = banks
        cycle_count = cycle_count + 1
    end do

    ! Print result
    print *, cycle_count

    deallocate(seen_configs)
end program day6