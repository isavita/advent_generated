
data = readline("input.txt")
target = parse(Int, strip(data))
target = target ÷ 10

houses = zeros(Int, target+1)

for elf in 1:target
    for house in elf:elf:target
        houses[house] += elf
    end
end

for (houseNumber, presents) in enumerate(houses)
    if presents >= target
        println(houseNumber)
        break
    end
end
