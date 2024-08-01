file = 'input.txt'
address = linein(file, 1)
target = strip(address)
target = target / 10

houses. = 0
do i = 1 to target
    houses.i = 0
end

do elf = 1 to target
    do house = elf to target by elf
        houses.house = houses.house + elf
    end
end

do houseNumber = 1 to target
    if houses.houseNumber >= target then do
        say houseNumber
        exit
    end
end