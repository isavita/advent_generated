program day06
    implicit none
    integer :: ios
    character(len=40) :: str
    logical :: lamps1(0:999,0:999) = .false.
    integer :: x_start, x_end, y_start, y_end
    integer :: x, y
    logical :: switch, toggle
    integer :: start, l2
    character(len=7) :: through
    integer :: counter1 = 0

    open(unit=99, file='input.txt', action='read', position='rewind')

    do
        read(99,'(A)',iostat=ios) str
        if (ios /= 0) exit
        select case (str(1:4))
        case ('turn')
            toggle = .false.
            select case (str(6:7))
            case ('on')
                start = 9
                switch = .true.
            case ('of')
                start = 10
                switch = .false.
            end select
        case ('togg')
            toggle = .true.
            start = 8
        end select

        read (str(start:), *) x_start, y_start, through, x_end, y_end

        do x = x_start, x_end
            do y = y_start, y_end
                if (toggle) then
                    lamps1(x,y) = .not.lamps1(x,y)
                else
                    lamps1(x,y) = switch
                end if
            end do
        end do
    end do

    close(99)

    do x = 0, 999
        do y = 0, 999
            if (lamps1(x, y)) counter1 = counter1 + 1
        end do
    end do

    print *, counter1

end program day06

