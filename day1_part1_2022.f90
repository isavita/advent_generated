program solution
    implicit none
    character(len=100) :: line
    integer :: maxCalories, currentCalories, calories
    integer :: fileUnit, iostat

    open(newunit=fileUnit, file='input.txt', status='old', action='read')

    maxCalories = 0
    currentCalories = 0

    do
        read(fileUnit, '(A)', iostat=iostat) line
        if (iostat /= 0) exit

        if (trim(line) == "") then
            if (currentCalories > maxCalories) then
                maxCalories = currentCalories
            end if
            currentCalories = 0
            cycle
        end if

        read(line, *) calories
        currentCalories = currentCalories + calories
    end do

    if (currentCalories > maxCalories) then
        maxCalories = currentCalories
    end if

    close(fileUnit)

    write(*, '(I0)') maxCalories
end program solution