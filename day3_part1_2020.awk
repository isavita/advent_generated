
{
    forest[NR] = $0
}
END {
    trees = countTrees(forest, 3, 1)
    print trees
}

function countTrees(forest, right, down) {
    trees = 0
    x = 0
    width = length(forest[1])

    for (y = 1; y <= length(forest); y += down) {
        if (substr(forest[y], x % width + 1, 1) == "#") {
            trees++
        }
        x += right
    }

    return trees
}
