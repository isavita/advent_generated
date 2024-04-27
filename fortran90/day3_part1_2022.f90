program rucksack
    implicit none
    character(len=100) :: line
    integer :: i, half, sum, priority
    logical :: exists
    character :: item
    integer, dimension(52) :: compartmentMap

    open(unit=10, file='input.txt', status='old', action='read')
    sum = 0

    do
        read(10, '(A)', end=10) line
        half = len_trim(line) / 2
        do i = 1, half
            item = line(i:i)
            select case (item)
            case ('a':'z')
                priority = ichar(item) - ichar('a') + 1
            case ('A':'Z')
                priority = ichar(item) - ichar('A') + 27
            end select
            compartmentMap(priority) = compartmentMap(priority) + 1
        end do
        do i = half + 1, len_trim(line)
            item = line(i:i)
            select case (item)
            case ('a':'z')
                priority = ichar(item) - ichar('a') + 1
            case ('A':'Z')
                priority = ichar(item) - ichar('A') + 27
            end select
            if (compartmentMap(priority) > 0) then
                sum = sum + priority
                exit
            end if
        end do
        compartmentMap = 0
    end do
    10 close(10)
    print *, sum
end program rucksack