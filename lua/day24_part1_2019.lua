function parse()
    local res = {}
    for i = 1, 25 do
        res[i] = false
    end

    local file = io.open("input.txt", "r")
    if not file then
        error("Failed to open file")
    end
    local row = 1
    for line in file:lines() do
        for col = 1, 5 do
            local index = (row - 1) * 5 + col
            if line:sub(col, col) == '#' then
                res[index] = true
            else
                res[index] = false
            end
        end
        row = row + 1
    end
    file:close()
    return res
end

function next1(grid)
    local newGrid = {}
    for i = 1, 25 do
        newGrid[i] = false
    end

    for i = 1, 25 do
        local row = math.floor((i - 1) / 5) + 1
        local col = (i - 1) % 5 + 1
        local neighbours = 0

        if row > 1 then
            if grid[i - 5] then
                neighbours = neighbours + 1
            end
        end
        if row < 5 then
            if grid[i + 5] then
                neighbours = neighbours + 1
            end
        end
        if col > 1 then
            if grid[i - 1] then
                neighbours = neighbours + 1
            end
        end
        if col < 5 then
            if grid[i + 1] then
                neighbours = neighbours + 1
            end
        end

        if grid[i] and neighbours ~= 1 then
            newGrid[i] = false
        elseif not grid[i] and (neighbours == 1 or neighbours == 2) then
            newGrid[i] = true
        else
            newGrid[i] = grid[i]
        end
    end

    return newGrid
end

function biodiversity(grid)
    local bio = 0
    for i = 1, 25 do
        if grid[i] then
            bio = bio + 2 ^ (i - 1)
        end
    end
    return bio
end

function gridToString(grid)
    local s = {}
    for i = 1, 25 do
        s[i] = grid[i] and '1' or '0'
    end
    return table.concat(s)
end

function main()
    local appeared = {}
    local grid = parse()
    local gridKey = gridToString(grid)
    appeared[gridKey] = true
    while true do
        grid = next1(grid)
        gridKey = gridToString(grid)
        if appeared[gridKey] then
            print(biodiversity(grid))
            return
        end
        appeared[gridKey] = true
    end
end

main()