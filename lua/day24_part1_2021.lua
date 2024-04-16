function read_file(file_path)
    local file = io.open(file_path, "r")
    if not file then return nil end
    local content = file:read("*all")
    file:close()
    return content
end

local input = read_file("input.txt")
local lines = {}
for line in input:gmatch("[^\r\n]+") do
    table.insert(lines, line)
end

local k, l, m = {}, {}, {}
for i, line in ipairs(lines) do
    local index = (i - 1) % 18
    if index == 4 then
        local value = line:match("div z (%-?%d+)")
        table.insert(l, tonumber(value))
    elseif index == 5 then
        local value = line:match("add x (%-?%d+)")
        table.insert(k, tonumber(value))
    elseif index == 15 then
        local value = line:match("add y (%-?%d+)")
        table.insert(m, tonumber(value))
    end
end

local constraints = {}
local stack = {}
for i, v in ipairs(l) do
    if v == 1 then
        table.insert(stack, i)
    elseif v == 26 then
        local pop = table.remove(stack)
        constraints[pop] = {i, m[pop] + k[i]}
    end
end

local max = {}
for i = 1, 14 do
    if constraints[i] then
        local vmax = 9
        while vmax + constraints[i][2] > 9 do
            vmax = vmax - 1
        end
        max[i] = vmax
        max[constraints[i][1]] = vmax + constraints[i][2]
    end
end

local function num(w)
    local n = 0
    for _, v in ipairs(w) do
        n = n * 10 + (v or 0)
    end
    return n
end

print(num(max))