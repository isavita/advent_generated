
local function solve(grid, part)
    local height = #grid
    local width = #grid[1]
    local start = {1, 1}
    local dest = {width, height}

    local directions = {N = {0, -1}, S = {0, 1}, E = {1, 0}, W = {-1, 0}}
    local direction_turns = {
        N = {L = 'W', R = 'E'},
        S = {L = 'E', R = 'W'},
        E = {L = 'N', R = 'S'},
        W = {L = 'S', R = 'N'}
    }

    local min_steps_before_turn, max_steps
    if part == 1 then
        min_steps_before_turn = 1
        max_steps = 3
    else
        min_steps_before_turn = 4
        max_steps = 10
    end

    local heap = {{0, start[1], start[2], 'E', 0}}
    local visited = {}

    local function push(h, v)
        table.insert(h, v)
        local i = #h
        while i > 1 and h[math.floor(i / 2)][1] > h[i][1] do
            h[i], h[math.floor(i / 2)] = h[math.floor(i / 2)], h[i]
            i = math.floor(i / 2)
        end
    end

    local function pop(h)
        local v = h[1]
        h[1] = h[#h]
        h[#h] = nil
        local i = 1
        while true do
            local smallest = i
            local left = 2 * i
            local right = 2 * i + 1
            if left <= #h and h[left][1] < h[smallest][1] then
                smallest = left
            end
            if right <= #h and h[right][1] < h[smallest][1] then
                smallest = right
            end
            if smallest ~= i then
                h[i], h[smallest] = h[smallest], h[i]
                i = smallest
            else
                break
            end
        end
        return v
    end
    
    while #heap > 0 do
        local heat_loss, x, y, dir, steps = table.unpack(pop(heap))

        if x == dest[1] and y == dest[2] then
            return heat_loss
        end

        local state_key = string.format("%d,%d,%s,%d", x, y, dir, steps)
        if visited[state_key] then
            goto continue
        end
        visited[state_key] = true

        if steps < max_steps then
            local dx, dy = directions[dir][1], directions[dir][2]
            local nx, ny = x + dx, y + dy
            if nx >= 1 and nx <= width and ny >= 1 and ny <= height then
                push(heap, {heat_loss + grid[ny][nx], nx, ny, dir, steps + 1})
            end
        end

        if steps >= min_steps_before_turn then
            for _, turn in ipairs({'L', 'R'}) do
                local new_dir = direction_turns[dir][turn]
                local dx, dy = directions[new_dir][1], directions[new_dir][2]
                local nx, ny = x + dx, y + dy
                if nx >= 1 and nx <= width and ny >= 1 and ny <= height then
                    push(heap, {heat_loss + grid[ny][nx], nx, ny, new_dir, 1})
                end
            end
        end

        ::continue::
    end

    return -1
end

local function main()
    local grid = {}
    for line in io.lines("input.txt") do
        local row = {}
        for char in line:gmatch(".") do
            table.insert(row, tonumber(char))
        end
        table.insert(grid, row)
    end

    print(solve(grid, 1))
    print(solve(grid, 2))
end

main()
