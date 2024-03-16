using DelimitedFiles

function main()
    fishes = zeros(Int, 9)
    input = readdlm("input.txt", ',', Int)
    for fish in input
        fishes[fish+1] += 1
    end

    for day in 1:80
        newFish = fishes[1]
        for i in 2:9
            fishes[i-1] = fishes[i]
        end
        fishes[7] += newFish
        fishes[9] = newFish
    end

    println(sum(fishes))
end

main()