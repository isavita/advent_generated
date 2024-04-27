program day6
    implicit none
    integer, parameter :: num_banks = 16
    integer :: banks(num_banks), max_bank, max_blocks, i, j, cycle, loop_size
    logical :: seen_before
    character(len=100) :: line
    integer, allocatable :: seen(:,:)

    ! Read input from file
    open(unit=10, file='input.txt', status='old')
    read(10, *) (banks(i), i=1, num_banks)
    close(10)

    ! Initialize variables
    cycle = 0
    loop_size = 0
    allocate(seen(num_banks, 10000))
    seen = -1

    do
        ! Find the bank with the most blocks
        max_bank = maxloc(banks, 1)
        max_blocks = banks(max_bank)

        ! Redistribute blocks
        banks(max_bank) = 0
        do i = 1, max_blocks
            max_bank = mod(max_bank, num_banks) + 1
            banks(max_bank) = banks(max_bank) + 1
        end do

        ! Check if this configuration has been seen before
        do i = 1, size(seen, 2)
            if (all(seen(:, i) == banks)) then
                loop_size = cycle - i + 1
                exit
            end if
        end do

        ! Store the current configuration
        seen(:, cycle + 1) = banks
        cycle = cycle + 1

        ! Check if we have found a loop
        if (loop_size /= 0) exit
    end do

    ! Print the answers
    print *, 'Number of cycles before a configuration is produced that has been seen before:', cycle
    print *, 'Size of the infinite loop:', loop_size

    deallocate(seen)
end program day6