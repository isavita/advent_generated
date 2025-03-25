
local math = require("math")

local function read_input(file_path)
    local walls = {}
    local blizzards = {}
    local file = io.open(file_path, "r")
    local y = 0
    local width = 0
    for line in file:lines() do
        line = line:gsub("%s", "")
        for x = 1, #line do
            local char = line:sub(x, x)
            if char == "#" then
                walls[x .. "," .. y] = true
            elseif char == ">" or char == "<" or char == "^" or char == "v" then
                table.insert(blizzards, {x = x, y = y, dir = char})
            end
            width = x
        end
        y = y + 1
    end
    io.close(file)
    return walls, blizzards, y, width
end

local function find_start_end(walls, height, width)
    local start_x, end_x
    for x = 1, width do
        if not walls[x .. "," .. 0] then
            start_x = x
            break
        end
    end
    for x = 1, width do
        if not walls[x .. "," .. (height - 1)] then
            end_x = x
            break
        end
    end
    return {x = start_x, y = 0}, {x = end_x, y = height - 1}
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
    end

    for t = 0, period - 1 do
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

local function bfs(start, end_pos, walls, blizzard_positions, period, width, height)
    local queue = {{x = start.x, y = start.y, t = 0}}
    local visited = {}
    visited[start.x .. "," .. start.y .. "," .. 0 % period] = true

    local directions = {{0, 0}, {1, 0}, {-1, 0}, {0, 1}, {0, -1}}

    local q_index = 1
    while q_index <= #queue do
        local current = queue[q_index]
        q_index = q_index + 1
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

            local state = nx .. "," .. ny .. "," .. next_t % period
            if not visited[state] then

                if (nx == start.x and ny == start.y) or (nx >= 1 and nx < width and ny >= 1 and ny < height) then
                    if not walls[nx .. "," .. ny] and not blizzards_next[nx .. "," .. ny] then
                        visited[state] = true
                        table.insert(queue, {x = nx, y = ny, t = next_t})
                    end
                end
            end
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
    local minutes = bfs(start, end_pos, walls, blizzard_positions, period, width, height)
    print(minutes)
end

main()
