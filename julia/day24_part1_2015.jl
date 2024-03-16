using Printf

function main()
    data = read("input.txt", String)
    lines = split(strip(data), "\n")
    packages = parse.(Int, lines)
    totalWeight = sum(packages)
    targetWeight = div(totalWeight, 3)
    bestQE = typemax(Int)
    bestLength = typemax(Int)

    for comb in 1:(2^length(packages)-1)
        groupWeight, qe, groupLength = 0, 1, 0
        for i in 1:length(packages)
            if comb & (1 << (i-1)) != 0
                groupWeight += packages[i]
                qe *= packages[i]
                groupLength += 1
            end
        end
        if groupWeight == targetWeight && groupLength <= bestLength
            if groupLength < bestLength || qe < bestQE
                bestLength = groupLength
                bestQE = qe
            end
        end
    end

    @printf("%d\n", bestQE)
end

main()