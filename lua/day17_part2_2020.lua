function simulateCycle4D(activeCubes)
    local newActiveCubes = {}
    local neighborCounts = {}

    for key, _ in pairs(activeCubes) do
        local coords = {}
        for coord in string.gmatch(key, "[^,]+") do
            table.insert(coords, tonumber(coord))
        end
        local coord = {x = coords[1], y = coords[2], z = coords[3], w = coords[4]}

        for dw = -1, 1 do
            for dz = -1, 1 do
                for dy = -1, 1 do
                    for dx = -1, 1 do
                        if dw ~= 0 or dz ~= 0 or dy ~= 0 or dx ~= 0 then
                            local neighbor = {x = coord.x + dx, y = coord.y + dy, z = coord.z + dz, w = coord.w + dw}
                            local neighborKey = neighbor.x .. "," .. neighbor.y .. "," .. neighbor.z .. "," .. neighbor.w
                            neighborCounts[neighborKey] = (neighborCounts[neighborKey] or 0) + 1
                        end
                    end
                end
            end
        end
    end

    for key, count in pairs(neighborCounts) do
        local coords = {}
        for coord in string.gmatch(key, "[^,]+") do
            table.insert(coords, tonumber(coord))
        end
        local coord = {x = coords[1], y = coords[2], z = coords[3], w = coords[4]}
        if count == 3 or (count == 2 and activeCubes[key]) then
            newActiveCubes[key] = true
        end
    end

    return newActiveCubes
end

function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("File reading error")
        return
    end

    local initialState = {}
    for line in file:lines() do
        table.insert(initialState, line)
    end
    file:close()

    local activeCubes = {}
    for y, line in ipairs(initialState) do
        for x = 1, #line do
            local char = line:sub(x, x)
            if char == '#' then
                local key = (x-1) .. "," .. (y-1) .. ",0,0"
                activeCubes[key] = true
            end
        end
    end

    for cycle = 1, 6 do
        activeCubes = simulateCycle4D(activeCubes)
    end

    print(table.count(activeCubes))
end

function table.count(t)
    local count = 0
    for _ in pairs(t) do
        count = count + 1
    end
    return count
end

main()