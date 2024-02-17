
program solution
    implicit none
    character(len=100) :: input
    integer :: cups(1000001)
    integer :: currentCup, i, cup, nextCup, firstCup, lastCup, pickup1, pickup2, pickup3, destinationCup

    open(unit=10, file='input.txt', status='old')
    read(10, '(A)') input
    close(10)

    currentCup = 0
    do i = 1, len_trim(input)
        read(input(i:i), '(I1)') cup
        if (i == 1) then
            currentCup = cup
        end if
        if (i < len_trim(input)) then
            read(input(i+1:i+1), '(I1)') nextCup
            cups(cup) = nextCup
        end if
    end do
    read(input(1:1), '(I1)') firstCup
    read(input(len_trim(input):len_trim(input)), '(I1)') lastCup
    cups(lastCup) = firstCup

    do i = 1, 100
        pickup1 = cups(currentCup)
        pickup2 = cups(pickup1)
        pickup3 = cups(pickup2)

        cups(currentCup) = cups(pickup3)

        destinationCup = currentCup - 1
        if (destinationCup < 1) then
            destinationCup = len_trim(input)
        end if
        do while (destinationCup == pickup1 .or. destinationCup == pickup2 .or. destinationCup == pickup3)
            destinationCup = destinationCup - 1
            if (destinationCup < 1) then
                destinationCup = len_trim(input)
            end if
        end do

        cups(pickup3) = cups(destinationCup)
        cups(destinationCup) = pickup1

        currentCup = cups(currentCup)
    end do

    cup = cups(1)
    do while (cup /= 1)
        write(*, '(I0)', advance='no') cup
        cup = cups(cup)
        if (cup == 1) then
            exit
        end if
    end do
    print *
end program solution
