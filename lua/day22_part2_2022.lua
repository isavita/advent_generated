
local P = function(x, y) return { x = x, y = y } end

local N = 0
local E = 1
local S = 2
local W = 3

local Dirs = {
    P(-1, 0),
    P(0, 1),
    P(1, 0),
    P(0, -1),
}

local Map = {}
local Size = 0
local Movements = {}

local function Rotate(dir, direction)
    if direction == 'R' then
        return (dir + 1) % 4
    elseif direction == 'L' then
        return (dir - 1 + 4) % 4
    end
    return dir
end

local function Points(dir)
    return (dir + 3) % 4
end

local function parsePath(path)
    local movements = {}
    local acc = 0
    for i = 1, #path do
        local char = path:sub(i, i)
        if char >= '0' and char <= '9' then
            acc = acc * 10 + tonumber(char)
        else
            if acc > 0 then
                table.insert(movements, { steps = acc })
            end
            acc = 0
            table.insert(movements, { rotate = char })
        end
    end
    if acc > 0 then
        table.insert(movements, { steps = acc })
    end
    return movements
end

local function crossBorder(n, dir)
    local x, y = n.x, n.y

    if x == -1 and y >= 0 and y < 2 * Size then return P(y + 2 * Size, 0), E
    elseif x == -1 and y >= 2 * Size then return P(4 * Size - 1, y - 2 * Size), N
    elseif x == Size and dir == S then return P(y - Size, x + Size - 1), W
    elseif x == 2 * Size - 1 and dir == N then return P(y + Size, x - Size + 1), E
    elseif x == 3 * Size and dir == S then return P(y + 2 * Size, x - 2 * Size - 1), W
    elseif x == 4 * Size then return P(n.x - 4 * Size, n.y + 2 * Size), S
    elseif n.y == -1 and n.x >= 0 and n.x < 3 * Size then return P(3 * Size - 1 - n.x, n.y + Size + 1), E
    elseif n.y == -1 and n.x >= 3 * Size then return P(n.y + 1, n.x - 2 * Size), S
    elseif n.y == Size - 1 and n.x >= 0 and n.x < Size then return P(3 * Size - 1 - n.x, n.y - Size + 1), E
    elseif n.y == Size - 1 and n.x >= Size and dir == W then return P(n.y + Size + 1, n.x - Size), S
    elseif n.y == Size and dir == E then return P(n.y + 2 * Size - 1, n.x - 2 * Size), N
    elseif n.y == 2 * Size and n.x >= 0 and n.x < 2 * Size and dir == E then return P(n.y - Size - 1, n.x + Size), N
    elseif n.y == 2 * Size and n.x >= 2 * Size then return P(3 * Size - 1 - n.x, n.y + Size - 1), W
    elseif n.y == 3 * Size then return P(3 * Size - 1 - n.x, n.y - Size - 1), W
    end

    error(string.format("Logic Error: Unhandled border crossing at P{%d, %d} from dir %d", n.x, n.y, dir))
end

local function parse()
    local file = io.open("input.txt", "r")
    if not file then
        error("Cannot open input.txt")
    end
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    local map_lines = {}
    local path_line = nil
    local found_empty_line = false

    for _, line in ipairs(lines) do
        if not found_empty_line then
            if line == "" then
                found_empty_line = true
            else
                table.insert(map_lines, line)
            end
        else
            if line ~= "" then
               path_line = line
               break
            end
        end
    end

    if #map_lines == 0 then
        error("No map found in input.txt")
    end

    Size = #map_lines[1] / 3

    for r_idx, line in ipairs(map_lines) do
        local r = r_idx - 1
        for c_idx = 1, #line do
            local char = line:sub(c_idx, c_idx)
            local c = c_idx - 1
            if char == '#' or char == '.' then
               Map[string.format("%d,%d", r, c)] = (char == '#')
            end
        end
    end

    if not path_line then
         error("No path found in input.txt")
    end
    Movements = parsePath(path_line)
end

local Human = {
    curr = nil,
    facing = E,
}

local function Walk(h)
    local dirDelta = Dirs[h.facing + 1]
    local next = P(h.curr.x + dirDelta.x, h.curr.y + dirDelta.y)
    local next_key = string.format("%d,%d", next.x, next.y)

    local is_wall = Map[next_key]

    if is_wall ~= nil then
        if is_wall then
            return false
        else
            h.curr = next
            return true
        end
    else
        local next_wrapped, nextFacing_wrapped = crossBorder(next, h.facing)
        local wrapped_key = string.format("%d,%d", next_wrapped.x, next_wrapped.y)

        local is_wall_wrapped = Map[wrapped_key]

        if is_wall_wrapped == nil then
             error(string.format("Logic Error: Cross border resulted in unmapped coordinate P{%d, %d}", next_wrapped.x, next_wrapped.y))
        end

        if is_wall_wrapped then
            return false
        else
            h.curr = next_wrapped
            h.facing = nextFacing_wrapped
            return true
        end
    end
end

local function main()
    parse()

    Human.curr = P(0, Size)

    for _, mov in ipairs(Movements) do
        if mov.rotate then
            Human.facing = Rotate(Human.facing, mov.rotate)
        elseif mov.steps then
            for i = 1, mov.steps do
                if not Walk(Human) then
                    break
                end
            end
        end
    end

    local final_score = 1000 * (Human.curr.x + 1) + 4 * (Human.curr.y + 1) + Points(Human.facing)
    print(final_score)
end

main()
