-- Read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local orbits = {}
    for line in file:lines() do
        local center, satellite = line:match("([^)]+)%)(.*)")
        orbits[satellite] = center
    end
    file:close()
    return orbits
end

-- Build the reverse orbit map (children for each object)
local function build_reverse_map(orbits)
    local reverse_map = {}
    for satellite, center in pairs(orbits) do
        if not reverse_map[center] then
            reverse_map[center] = {}
        end
        table.insert(reverse_map[center], satellite)
    end
    return reverse_map
end

-- Breadth-First Search to find shortest path
local function bfs(orbits, reverse_map, start, goal)
    local queue = {{start, 0}}
    local visited = {[start] = true}
    
    while #queue > 0 do
        local current, distance = table.unpack(table.remove(queue, 1))
        
        if current == goal then
            return distance - 2  -- Subtract 2 because we don't count YOU and SAN
        end
        
        local neighbors = reverse_map[current] or {}
        if orbits[current] then
            table.insert(neighbors, orbits[current])
        end
        
        for _, neighbor in ipairs(neighbors) do
            if not visited[neighbor] then
                visited[neighbor] = true
                table.insert(queue, {neighbor, distance + 1})
            end
        end
    end
    
    return -1  -- Path not found
end

-- Main function
local function main()
    local orbits = read_input()
    local reverse_map = build_reverse_map(orbits)
    
    local transfers = bfs(orbits, reverse_map, orbits["YOU"], orbits["SAN"])
    
    print("Minimum number of orbital transfers required:", transfers)
end

main()
