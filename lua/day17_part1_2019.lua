local function readInput(filename)
    local file = io.open(filename, "r")
    if not file then error("Failed to open file: " .. filename) end
    local content = file:read("*all")
    file:close()
    return content
end

local function split(inputstr, sep)
    if sep == nil then
        sep = "%s"
    end
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, str)
    end
    return t
end

local function atoi(s)
    return tonumber(s)
end

local function decode(n)
    local op = n % 100
    local modes = {}
    n = math.floor(n / 100)
    for i = 1, 3 do
        table.insert(modes, n % 10)
        n = math.floor(n / 10)
    end
    return op, modes
end

local function get(m, i, mode)
    if mode == 1 then
        return m.data[i]
    elseif mode == 0 then
        return m.data[m.data[i]] or 0
    elseif mode == 2 then
        return m.data[m.relbase + m.data[i]] or 0
    end
    return 0
end

local function set(m, i, mode, val)
    if mode == 0 then
        m.data[m.data[i]] = val
    elseif mode == 2 then
        m.data[m.relbase + m.data[i]] = val
    end
end

local function machineStep(m)
    local op, modes = decode(m.data[m.ip])
    if op == 1 or op == 2 then
        local val1 = get(m, m.ip + 1, modes[1])
        local val2 = get(m, m.ip + 2, modes[2])
        if op == 1 then
            set(m, m.ip + 3, modes[3], val1 + val2)
        else
            set(m, m.ip + 3, modes[3], val1 * val2)
        end
        m.ip = m.ip + 4
    elseif op == 3 then
        set(m, m.ip + 1, modes[1], table.remove(m.input))
        m.ip = m.ip + 2
    elseif op == 4 then
        table.insert(m.output, get(m, m.ip + 1, modes[1]))
        m.ip = m.ip + 2
    elseif op == 5 or op == 6 then
        local val = get(m, m.ip + 1, modes[1])
        if (op == 5 and val ~= 0) or (op == 6 and val == 0) then
            m.ip = get(m, m.ip + 2, modes[2])
        else
            m.ip = m.ip + 3
        end
    elseif op == 7 or op == 8 then
        local val1 = get(m, m.ip + 1, modes[1])
        local val2 = get(m, m.ip + 2, modes[2])
        if (op == 7 and val1 < val2) or (op == 8 and val1 == val2) then
            set(m, m.ip + 3, modes[3], 1)
        else
            set(m, m.ip + 3, modes[3], 0)
        end
        m.ip = m.ip + 4
    elseif op == 9 then
        m.relbase = m.relbase + get(m, m.ip + 1, modes[1])
        m.ip = m.ip + 2
    elseif op == 99 then
        return false
    end
    return true
end

local function runMachine(program, input)
    local m = {
        data = {},
        ip = 0,
        input = input,
        output = {},
        relbase = 0
    }
    for i = 1, #program do
        m.data[i - 1] = program[i]
    end
    while machineStep(m) do end
    return m.output
end

local function parse(program)
    local out = runMachine(program, {})
    local scaffolding = {}
    local robot, dir
    local y = 0
    local x = 0  -- Initialize x here
    for i, v in ipairs(out) do
        local c = string.char(v)
        if c == '\n' then
            y = y + 1
            x = 0
        else
            if c == '#' then
                scaffolding[y * 1000 + x] = true
            elseif c == '^' or c == 'v' or c == '<' or c == '>' then
                robot = {x = x, y = y}
                dir = c
                scaffolding[y * 1000 + x] = true
            end
            x = x + 1
        end
    end
    return scaffolding, robot, dir
end

local function sumAlign(grid)
    local sum = 0
    for p in pairs(grid) do
        local x = p % 1000
        local y = math.floor(p / 1000)
        local count = 0
        for _, n in ipairs({-1, 1, -1000, 1000}) do
            if grid[p + n] then count = count + 1 end
        end
        if count == 4 then
            sum = sum + x * y
        end
    end
    return sum
end

local input = readInput("input.txt")
local program = {}
for _, n in ipairs(split(input, ",")) do
    table.insert(program, atoi(n))
end
local scaffolding, robot, dir = parse(program)
print(sumAlign(scaffolding))