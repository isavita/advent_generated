function buildMap(regex)
    local dm = {}
    local stack = {}
    local cp = {x = 0, y = 0}
    
    for c in regex:gmatch(".") do
        if c == '(' then
            table.insert(stack, cp)
        elseif c == '|' then
            cp = stack[#stack]
        elseif c == ')' then
            cp = stack[#stack]
            table.remove(stack)
        else
            local np = move(cp, c)
            if not dm[cp.x] then dm[cp.x] = {} end
            if not dm[cp.x][cp.y] then dm[cp.x][cp.y] = {} end
            dm[cp.x][cp.y][np] = true
            cp = np
        end
    end
    
    return dm
end

function move(p, dir)
    if dir == 'N' then return {x = p.x, y = p.y - 1}
    elseif dir == 'S' then return {x = p.x, y = p.y + 1}
    elseif dir == 'E' then return {x = p.x + 1, y = p.y}
    elseif dir == 'W' then return {x = p.x - 1, y = p.y}
    end
    return p
end

function findFurthestRoom(dm)
    local visited = {}
    local queue = {}
    table.insert(queue, {x = 0, y = 0})
    local maxDoors = 0

    while #queue > 0 do
        local p = table.remove(queue, 1)
        local px, py = p.x, p.y
        if dm[px] and dm[px][py] then
            for np, _ in pairs(dm[px][py]) do
                if not visited[np.x] then visited[np.x] = {} end
                if not visited[np.x][np.y] then
                    visited[np.x][np.y] = (visited[px][py] or 0) + 1
                    maxDoors = math.max(maxDoors, visited[np.x][np.y])
                    table.insert(queue, np)
                end
            end
        end
    end
    
    return maxDoors
end

function main()
    local file = io.open("input.txt", "r")
    local regex = file:read("*a")
    file:close()
    regex = regex:sub(2, #regex - 1)
    
    local dm = buildMap(regex)
    local maxDoors = findFurthestRoom(dm)
    print(maxDoors)
end

main()