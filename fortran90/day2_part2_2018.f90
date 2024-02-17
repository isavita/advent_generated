
program solution
    character(255) :: lines(1000)
    character(255) :: common
    integer :: i, j, k, diff, ios
    open(unit=10, file='input.txt', status='old', action='read')
    do i = 1, 1000
        read(10, '(A)', IOSTAT=ios) lines(i)
        if (ios /= 0) exit
    end do
    close(10)
    do i = 1, 999
        do j = i + 1, 1000
            diff = 0
            do k = 1, min(len_trim(lines(i)), len_trim(lines(j)))
                if (lines(i)(k:k) /= lines(j)(k:k)) then
                    diff = diff + 1
                    if (diff > 1) exit
                end if
            end do
            if (diff == 1) then
                common = ''
                do k = 1, min(len_trim(lines(i)), len_trim(lines(j)))
                    if (lines(i)(k:k) == lines(j)(k:k)) then
                        common = trim(common) // lines(i)(k:k)
                    end if
                end do
                write(*, '(A)') trim(common)
                stop
            end if
        end do
    end do
end program solution
