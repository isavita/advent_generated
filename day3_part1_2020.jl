function countTrees(forest, right, down)
    trees = 0
    x = 1
    width = length(forest[1])
    
    for y in 1:down:length(forest)
        if forest[y][x] == '#'
            trees += 1
        end
        x = (x + right - 1) % width + 1
    end
    
    return trees
end

forest = readlines("input.txt")
trees = countTrees(forest, 3, 1)
println(trees)