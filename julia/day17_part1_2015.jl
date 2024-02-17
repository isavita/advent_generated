
function countCombinations(containers, target, index)
    if target == 0
        return 1
    end
    if target < 0 || index > length(containers)
        return 0
    end
    return countCombinations(containers, target - containers[index], index + 1) +
           countCombinations(containers, target, index + 1)
end

containers = parse.(Int, readlines("input.txt"))
println(countCombinations(containers, 150, 1))
