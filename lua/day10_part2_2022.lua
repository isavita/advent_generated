
local function read_file(path)
    local file = io.open(path, "r")
    local content = file:read("*a")
    file:close()
    return content
end

local function abs(x)
    return x < 0 and -x or x
end

local input = read_file("input.txt")
local lines = {}
for line in input:gmatch("[^\r\n]+") do
    table.insert(lines, line)
end

local x = {1}
for _, line in ipairs(lines) do
    if line == "noop" then
        table.insert(x, x[#x])
    else
        local n = tonumber(line:match("addx (%-?%d+)"))
        table.insert(x, x[#x])
        table.insert(x, x[#x] + n)
    end
end

local grid = {}
for i = 1, #x do
    local crtx, crty = (i - 1) % 40, math.floor((i - 1) / 40)
    if abs(crtx - x[i]) <= 1 then
        grid[crty * 40 + crtx] = true
    end
end

for y = 0, 5 do
    for x = 0, 39 do
        io.write(grid[y * 40 + x] and "#" or ".")
    end
    io.write("\n")
end
