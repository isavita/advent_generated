function checkSequence(scoreboard, sequence)
    if length(scoreboard) < length(sequence)
        return false
    end
    start = length(scoreboard) - length(sequence)
    for i in 1:length(sequence)
        if scoreboard[start+i] != sequence[i]
            return false
        end
    end
    return true
end

function main()
    input = readline("input.txt")
    scoreboard = [3, 7]
    elf1, elf2 = 1, 2
    inputLen = length(input)
    inputSequence = [parse(Int, c) for c in input]

    while true
        newScore = scoreboard[elf1] + scoreboard[elf2]
        if newScore >= 10
            push!(scoreboard, div(newScore, 10))
            if checkSequence(scoreboard, inputSequence)
                break
            end
        end
        push!(scoreboard, newScore % 10)
        if checkSequence(scoreboard, inputSequence)
            break
        end

        elf1 = (elf1 + scoreboard[elf1]) % length(scoreboard) + 1
        elf2 = (elf2 + scoreboard[elf2]) % length(scoreboard) + 1
    end

    println(length(scoreboard) - inputLen)
end

main()