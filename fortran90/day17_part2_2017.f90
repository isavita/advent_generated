
program solution
    implicit none
    integer :: steps, currentPos, valueAfterZero, i
    character(len=10) :: line

    open(unit=10, file='input.txt', status='old')
    read(10, '(A)') line
    close(10)

    read(line, '(I10)') steps
    currentPos = 0
    valueAfterZero = 0

    do i = 1, 50000000
        currentPos = mod(currentPos + steps, i)
        if (currentPos == 0) then
            valueAfterZero = i
        end if
        currentPos = currentPos + 1
    end do

    print*, valueAfterZero
end program solution
