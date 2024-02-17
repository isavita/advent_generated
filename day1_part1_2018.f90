program solution
    implicit none
    character(len=100) :: line
    integer :: freqChanges(100)
    integer :: freq, i, sign, num, err

    open(unit=10, file='input.txt', status='old')
    i = 1
    do
        read(10, '(A)', iostat=err) line
        if (err /= 0) exit
        read(line, '(I)') freqChanges(i)
        i = i + 1
    end do
    close(10)

    freq = 0
    do i = 1, size(freqChanges)
        call parseChange(freqChanges(i), sign, num, err)
        freq = freq + sign * num
    end do

    print *, freq

contains

    subroutine parseChange(change, sign, num, err)
        character(len=100), intent(in) :: change
        integer, intent(out) :: sign, num, err

        sign = 1
        if (change(1:1) == '-') then
            sign = -1
            change = adjustl(change(2:))
        endif
        read(change, '(I)', iostat=err) num
        if (err /= 0) then
            write(*, '(A)', advance='no') 'invalid frequency change: ', change
            stop
        endif
    end subroutine parseChange

end program solution