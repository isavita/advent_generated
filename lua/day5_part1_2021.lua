local function main()
    local file = io.open("input.txt", "r")
    local grid = {}

    for line in file:lines() do
        local parts = {}
        for part in line:gmatch("[^%s]+") do
            table.insert(parts, part)
        end

        local startCoords = {}
        for num in parts[1]:gmatch("[^,]+") do
            table.insert(startCoords, tonumber(num))
        end

        local endCoords = {}
        for num in parts[3]:gmatch("[^,]+") do
            table.insert(endCoords, tonumber(num))
        end

        local x1, y1 = startCoords[1], startCoords[2]
        local x2, y2 = endCoords[1], endCoords[2]

        if x1 == x2 then
            if y1 > y2 then
                y1, y2 = y2, y1
            end
            for y = y1, y2 do
                grid[x1] = grid[x1] or {}
                grid[x1][y] = (grid[x1][y] or 0) + 1
            end
        elseif y1 == y2 then
            if x1 > x2 then
                x1, x2 = x2, x1
            end
            for x = x1, x2 do
                grid[x] = grid[x] or {}
                grid[x][y1] = (grid[x][y1] or 0) + 1
            end
        end
    end

    local overlapCount = 0
    for _, column in pairs(grid) do
        for _, count in pairs(column) do
            if count > 1 then
                overlapCount = overlapCount + 1
            end
        end
    end

    print(overlapCount)
    file:close()
end

main()