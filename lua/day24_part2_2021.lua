function read_all(file_path)
    local file = io.open(file_path, "r")
    if not file then error("File not found") end
    local content = file:read("*all")
    file:close()
    return content
end

function parse_input(input)
    local k, l, m = {}, {}, {}
    local lines = {}
    for line in input:gmatch("[^\n]+") do
        table.insert(lines, line)
    end

    for i, line in ipairs(lines) do
        local index = (i - 1) % 18
        if index == 4 then
            local v = line:match("div z (%d+)")
            table.insert(l, tonumber(v) or 1)  -- Default to 1 if parsing fails
        elseif index == 5 then
            local v = line:match("add x (-?%d+)")
            table.insert(k, tonumber(v) or 0)  -- Default to 0 if parsing fails
        elseif index == 15 then
            local v = line:match("add y (-?%d+)")
            table.insert(m, tonumber(v) or 0)  -- Default to 0 if parsing fails
        end
    end
    return k, l, m
end

function solve(k, l, m)
    local constraints = {}
    local stack = {}
    for i = 1, #l do
        if l[i] == 1 then
            table.insert(stack, i)
        elseif l[i] == 26 then
            local pop = table.remove(stack)
            constraints[pop] = {i, m[pop] + k[i]}
        end
    end

    local min = {}
    for i = 1, 14 do
        if constraints[i] then
            local vmin = 1
            local target = 1 - constraints[i][2]
            if target > 1 then
                vmin = target
            end
            min[i] = vmin
            min[constraints[i][1]] = vmin + constraints[i][2]
        end
    end
    return num(min)
end

function num(w)
    local n = 0
    for i = 1, #w do
        n = n * 10 + (w[i] or 1) -- default to 1 if not set
    end
    return n
end

local input = read_all("input.txt")
local k, l, m = parse_input(input)
local result = solve(k, l, m)
print(result)