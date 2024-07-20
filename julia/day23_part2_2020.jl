
const totalCups = 1_000_000
const totalMoves = 10_000_000

function main()
    input = read("input.txt", String)
    cups = zeros(Int, totalCups + 1)
    lastCup = 0

    for (i, char) in enumerate(input)
        cup = parse(Int, char)
        if i > 1
            cups[lastCup + 1] = cup
        end
        lastCup = cup
    end

    for i in (length(input) + 1):totalCups
        cups[lastCup + 1] = i
        lastCup = i
    end
    cups[lastCup + 1] = parse(Int, input[1])

    currentCup = parse(Int, input[1])
    for _ in 1:totalMoves
        pickup1 = cups[currentCup + 1]
        pickup2 = cups[pickup1 + 1]
        pickup3 = cups[pickup2 + 1]
        cups[currentCup + 1] = cups[pickup3 + 1]

        destinationCup = currentCup - 1
        if destinationCup == 0
            destinationCup = totalCups
        end
        while destinationCup in (pickup1, pickup2, pickup3)
            destinationCup -= 1
            if destinationCup == 0
                destinationCup = totalCups
            end
        end

        cups[pickup3 + 1] = cups[destinationCup + 1]
        cups[destinationCup + 1] = pickup1

        currentCup = cups[currentCup + 1]
    end

    cup1 = cups[2]
    cup2 = cups[cup1 + 1]
    println(cup1 * cup2)
end

main()
