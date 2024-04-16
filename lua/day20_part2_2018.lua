local function read_file(file_path)
    local file = io.open(file_path, "r")
    if not file then error("File not found") end
    local content = file:read("*a")
    file:close()
    return content
end

local function move(point, dir)
    if dir == 'N' then return {x = point.x, y = point.y - 1}
    elseif dir == 'S' then return {x = point.x, y = point.y + 1}
    elseif dir == 'E' then return {x = point.x + 1, y = point.y}
    elseif dir == 'W' then return {x = point.x - 1, y = point.y}
    else return point
    end
end

local function build_map(regex)
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
            if not dm[cp.x][cp.y][np.x] then dm[cp.x][cp.y][np.x] = {} end
            dm[cp.x][cp.y][np.x][np.y] = true
            cp = np
        end
    end
    return dm
end

local function count_rooms(dm, min_doors)
    local visited = {}
    local queue = {{x = 0, y = 0}}
    local room_count = 0
    
    while #queue > 0 do
        local p = table.remove(queue, 1)
        if not visited[p.x] then visited[p.x] = {} end
        if not visited[p.x][p.y] then visited[p.x][p.y] = 0 end
        
        for x, ys in pairs(dm[p.x][p.y] or {}) do
            for y, _ in pairs(ys) do
                if not visited[x] or not visited[x][y] then
                    if not visited[x] then visited[x] = {} end
                    visited[x][y] = visited[p.x][p.y] + 1
                    if visited[x][y] >= min_doors then
                        room_count = room_count + 1
                    end
                    table.insert(queue, {x = x, y = y})
                end
            end
        end
    end
    return room_count
end

local regex = read_file("input.txt")
local dm = build_map(regex:sub(2, -2))
local rooms = count_rooms(dm, 1000)
print(rooms)