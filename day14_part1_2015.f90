program day14
    implicit none
    integer, parameter :: race_time = 2503
    type reindeer
        integer :: speed, flight_time, rest_time, distance, rem, points
        logical :: status
    end type reindeer

    character(len=80) :: line
    integer :: i, j
    integer :: input_length = 0
    integer :: ios
    type(reindeer), allocatable :: reindeers(:)
    integer :: distance, rem_time, maximum = 0
    logical, parameter :: running = .true., resting = .false.

    open(unit = 99, file = 'input.txt', action = 'read', position = 'rewind')

    do 
        read(99, '(A)', iostat = ios)
        if (ios /= 0) exit
        input_length = input_length + 1
    end do

    allocate(reindeers(input_length))
    rewind(99)

    do i = 1, input_length
        reindeers(i)%distance = 0
        reindeers(i)%points = 0
        reindeers(i)%status = running
        read(99, '(A)') line
        j = 1
        do while (line(j:j) /= ' ')
            j = j + 1
        end do
        j = j + 9
        call read_int(line, j, reindeers(i)%speed)
        j = j + 10
        call read_int(line, j, reindeers(i)%flight_time)
        reindeers(i)%rem = reindeers(i)%flight_time
        j = j + 33
        call read_int(line, j, reindeers(i)%rest_time)
    end do

    close(99)

    do i = 1, input_length
        distance = 0
        rem_time = race_time

        do while (rem_time > 0)
            distance = distance + (reindeers(i)%speed * reindeers(i)%flight_time)
            rem_time = rem_time - reindeers(i)%flight_time - reindeers(i)%rest_time
        end do
        maximum = max(distance, maximum)
    end do

    print *, maximum


contains

    subroutine read_int(line, i, n)
        character(len=80), intent(in) :: line
        integer, intent(inout) :: i
        integer, intent(inout) :: n
        integer :: j

        n = 0; j = 1
        do
            if (line(i:i) == ' ') exit
            n = n * 10 + iachar(line(i:i)) - 48
            i = i + 1; j = j + 1
        end do
    end subroutine read_int


end program day14
