program main
    implicit none
    integer :: i, total, mass
    character(len=10) :: line
    open(unit=10, file='input.txt', status='old')

    total = 0
    do
        read(10, '(A)', end=10) line
        read(line, *) mass
        total = total + calcFuelMass(mass)
    end do
10  close(10)
    print *, total

contains
    recursive function calcFuelMass(mass) result(fuel)
        integer, intent(in) :: mass
        integer :: fuel
        fuel = max(0, (mass / 3) - 2)
        if (fuel > 0) fuel = fuel + calcFuelMass(fuel)
    end function calcFuelMass
end program main