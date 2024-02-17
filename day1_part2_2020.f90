program solution
    integer :: expenses(200)
    integer :: i, j, k, num

    open(unit=10, file='input.txt', status='old')
    
    do i = 1, 200
        read(10,*) num
        expenses(i) = num
    end do

    do i = 1, 200
        do j = i+1, 200
            do k = j+1, 200
                if (expenses(i) + expenses(j) + expenses(k) == 2020) then
                    write(*,*) expenses(i) * expenses(j) * expenses(k)
                    stop
                end if
            end do
        end do
    end do

    close(10)
end program solution