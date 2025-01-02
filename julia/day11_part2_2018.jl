
function solve()
    serial = parse(Int, strip(read("input.txt", String)))
    gridSize = 300
    grid = zeros(Int, gridSize, gridSize)

    for y in 1:gridSize, x in 1:gridSize
        rackID = x + 10
        powerLevel = rackID * y
        powerLevel += serial
        powerLevel *= rackID
        powerLevel = (powerLevel ÷ 100) % 10
        powerLevel -= 5
        grid[y, x] = powerLevel
    end

    maxPower = -typemax(Int)
    maxX, maxY, maxSize = 0, 0, 0

    for size in 1:gridSize
        for y in 1:(gridSize - size + 1), x in 1:(gridSize - size + 1)
            totalPower = sum(grid[y:y+size-1, x:x+size-1])
            if totalPower > maxPower
                maxPower = totalPower
                maxX, maxY, maxSize = x, y, size
            end
        end
    end

    println("$(maxX),$(maxY),$(maxSize)")
end

solve()
