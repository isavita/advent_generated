
program solution
    implicit none
    integer :: totalElves, highestPowerOfTwo, winner
    open(unit=10, file='input.txt', status='old')
    read(10, *) totalElves
    close(10)
    
    highestPowerOfTwo = 1
    do while (highestPowerOfTwo*2 <= totalElves)
        highestPowerOfTwo = highestPowerOfTwo * 2
    end do
    
    winner = (totalElves - highestPowerOfTwo) * 2 + 1
    print *, winner
end program solution
