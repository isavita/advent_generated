
local math = require("math")

local function read_input(file_path)
    local walls = {}
    local blizzards = {}
    local file = io.open(file_path, "r")
    local height = 0
    local width = 0

    if file then
        for line in file:lines() do
            local y = height
            height = height + 1
            width = #line
            for x = 1, width do
                local char = string.sub(line, x, x)
                if char == "#" then
                    walls[x .. "," .. y] = true
                elseif char == ">" or char == "<" or char == "^" or char == "v" then
                    table.insert(blizzards, { x = x, y = y, dir = char })
                end
            end
        end
        io.close(file)
    else
        print("Error opening file")
        return nil, nil, nil, nil
    end

    return walls, blizzards, height, width
end

local function find_start_end(walls, height, width)
    local start_x, start_y = nil, 0
    for x = 1, width do
        if not walls[x .. "," .. start_y] then
            start_x = x
            break
        end
    end
    local end_x, end_y = nil, height - 1
    for x = 1, width do
        if not walls[x .. "," .. end_y] then
            end_x = x
            break
        end
    end
    return { x = start_x, y = start_y }, { x = end_x, y = end_y }
end

local function gcd(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

local function lcm(a, b)
    return math.abs(a * b) / gcd(a, b)
end

local function compute_period(width, height)
    return lcm(width, height)
end

local function precompute_blizzards(blizzards, width, height, period)
    local blizzard_positions = {}
    for t = 0, period - 1 do
        blizzard_positions[t] = {}
        for _, b in ipairs(blizzards) do
            local x, y, dir = b.x, b.y, b.dir
            local new_x, new_y
            if dir == ">" then
                new_x = 1 + ((x - 1 + t) % (width - 2))
                new_y = y
            elseif dir == "<" then
                new_x = 1 + ((x - 1 - t) % (width - 2))
                new_y = y
            elseif dir == "v" then
                new_x = x
                new_y = 1 + ((y - 1 + t) % (height - 2))
            elseif dir == "^" then
                new_x = x
                new_y = 1 + ((y - 1 - t) % (height - 2))
            end
            blizzard_positions[t][new_x .. "," .. new_y] = true
        end
    end
    return blizzard_positions
end

local function bfs(start, end_pos, walls, blizzard_positions, period, width, height, start_time)
    local queue = {{ x = start.x, y = start.y, t = start_time }}
    local visited = {}
    visited[start.x .. "," .. start.y .. "," .. (start_time % period)] = true

    local directions = {{0, 0}, {1, 0}, {-1, 0}, {0, 1}, {0, -1}}

    while #queue > 0 do
        local current = table.remove(queue, 1)
        local x, y, t = current.x, current.y, current.t

        if x == end_pos.x and y == end_pos.y then
            return t
        end

        local next_t = t + 1
        local blizzards_next = blizzard_positions[next_t % period]

        for _, dir in ipairs(directions) do
            local dx, dy = dir[1], dir[2]
            local nx, ny = x + dx, y + dy

            if nx == end_pos.x and ny == end_pos.y then
                return next_t
            end
              if nx == start.x and ny == start.y then
                 local state = nx .. "," .. ny .. "," .. (next_t % period)
                if not visited[state] and not blizzards_next[nx .. "," .. ny] then
                   visited[state] = true
                   table.insert(queue, {x = nx, y = ny, t = next_t})
                end
                  goto continue
             end
            if nx >= 1 and nx < width and ny >= 0 and ny < height then
                if not walls[nx .. "," .. ny] and not blizzards_next[nx .. "," .. ny] then
                    local state = nx .. "," .. ny .. "," .. (next_t % period)
                    if not visited[state] then
                        visited[state] = true
                        table.insert(queue, {x = nx, y = ny, t = next_t})
                    end
                end
            end
            ::continue::
        end
    end

    return -1
end

local function main()
    local input_file = "input.txt"
    local walls, blizzards, height, width = read_input(input_file)
    local start, end_pos = find_start_end(walls, height, width)
    local period = compute_period(width - 2, height - 2)
    local blizzard_positions = precompute_blizzards(blizzards, width, height, period)

    local time1 = bfs(start, end_pos, walls, blizzard_positions, period, width, height, 0)
    if time1 == -1 then
        print("No path found for trip 1.")
        return
    end

    local time2 = bfs(end_pos, start, walls, blizzard_positions, period, width, height, time1)
    if time2 == -1 then
        print("No path found for trip 2.")
        return
    end

    local time3 = bfs(start, end_pos, walls, blizzard_positions, period, width, height, time2)
    if time3 == -1 then
        print("No path found for trip 3.")
        return
    end

    print(time3)
end

main()
