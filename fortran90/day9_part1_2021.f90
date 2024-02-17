program solution
    implicit none
    integer :: heightmap(100,100)
    integer :: x, y, height, totalRiskLevel
    integer :: i, j, n, m
    character(len=100) :: line

    open(unit=10, file='input.txt', status='old')
    n = 0
    do
        read(10, '(A)', iostat=m) line
        if (m /= 0) exit
        n = n + 1
        do i = 1, len_trim(line)
            height = ichar(line(i:i)) - ichar('0')
            heightmap(n,i) = height
        end do
    end do
    close(10)

    totalRiskLevel = 0
    do y = 1, n
        do x = 1, len_trim(line)
            if (isLowPoint(heightmap, x, y, n)) then
                totalRiskLevel = totalRiskLevel + 1 + heightmap(y,x)
            end if
        end do
    end do

    print *, totalRiskLevel

contains

    logical function isLowPoint(heightmap, x, y, n)
        integer, intent(in) :: heightmap(n,n)
        integer, intent(in) :: x, y, n
        integer :: height

        height = heightmap(y,x)
        if (x > 1 .and. heightmap(y,x-1) <= height) then
            isLowPoint = .false.
            return
        end if
        if (x < n .and. heightmap(y,x+1) <= height) then
            isLowPoint = .false.
            return
        end if
        if (y > 1 .and. heightmap(y-1,x) <= height) then
            isLowPoint = .false.
            return
        end if
        if (y < n .and. heightmap(y+1,x) <= height) then
            isLowPoint = .false.
            return
        end if
        isLowPoint = .true.
    end function isLowPoint

end program solution