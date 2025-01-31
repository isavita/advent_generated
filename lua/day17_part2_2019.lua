
local function read_file(filepath)
    local file = io.open(filepath, "r")
    if not file then
        error("File not found")
    end
    local content = file:read("*a")
    file:close()
    return content:gsub("%s+", "")
end

local function split_string(str, delimiter)
    local result = {}
    for match in (str .. delimiter):gmatch("(.-)" .. delimiter) do
        table.insert(result, match)
    end
    return result
end

local function parse_program(input)
    local program_str = split_string(input, ",")
    local program = {}
    for _, n in ipairs(program_str) do
        table.insert(program, tonumber(n))
    end
    return program
end

local function decode(n)
    local op = n % 100
    n = math.floor(n / 100)
    local modes = {}
    for i = 1, 3 do
        modes[i] = n % 10
        n = math.floor(n / 10)
    end
    return op, modes
end

local Machine = {}
Machine.__index = Machine

function Machine.new(program, in_func, out_func)
    local self = setmetatable({}, Machine)
    self.data = {}
    for i, n in ipairs(program) do
        self.data[i - 1] = n
    end
    self.ip = 0
    self.in_func = in_func
    self.out_func = out_func
    self.relbase = 0
    return self
end

function Machine:get(i, mo)
    if mo == 1 then
        return self.data[i] or 0
    elseif mo == 0 then
        return self.data[self.data[i] or 0] or 0
    elseif mo == 2 then
        return self.data[self.relbase + (self.data[i] or 0)] or 0
    end
end

function Machine:set(i, mo, val)
    if mo == 0 then
        self.data[self.data[i] or 0] = val
    elseif mo == 2 then
        self.data[self.relbase + (self.data[i] or 0)] = val
    end
end

function Machine:step()
    local op, modes = decode(self.data[self.ip])
    if op == 1 then
        local val = self:get(self.ip + 1, modes[1]) + self:get(self.ip + 2, modes[2])
        self:set(self.ip + 3, modes[3], val)
        self.ip = self.ip + 4
    elseif op == 2 then
        local val = self:get(self.ip + 1, modes[1]) * self:get(self.ip + 2, modes[2])
        self:set(self.ip + 3, modes[3], val)
        self.ip = self.ip + 4
    elseif op == 3 then
        self:set(self.ip + 1, modes[1], self.in_func())
        self.ip = self.ip + 2
    elseif op == 4 then
        self.out_func(self:get(self.ip + 1, modes[1]))
        self.ip = self.ip + 2
    elseif op == 5 then
        if self:get(self.ip + 1, modes[1]) ~= 0 then
            self.ip = self:get(self.ip + 2, modes[2])
        else
            self.ip = self.ip + 3
        end
    elseif op == 6 then
        if self:get(self.ip + 1, modes[1]) == 0 then
            self.ip = self:get(self.ip + 2, modes[2])
        else
            self.ip = self.ip + 3
        end
    elseif op == 7 then
        if self:get(self.ip + 1, modes[1]) < self:get(self.ip + 2, modes[2]) then
            self:set(self.ip + 3, modes[3], 1)
        else
            self:set(self.ip + 3, modes[3], 0)
        end
        self.ip = self.ip + 4
    elseif op == 8 then
        if self:get(self.ip + 1, modes[1]) == self:get(self.ip + 2, modes[2]) then
            self:set(self.ip + 3, modes[3], 1)
        else
            self:set(self.ip + 3, modes[3], 0)
        end
        self.ip = self.ip + 4
    elseif op == 9 then
        self.relbase = self.relbase + self:get(self.ip + 1, modes[1])
        self.ip = self.ip + 2
    elseif op == 99 then
        return false
    end
    return true
end

function Machine:run()
    while self:step() do
    end
end

local function run_program(program, in_func, out_func)
    local m = Machine.new(program, in_func, out_func)
    m:run()
end

local point = {
    N = { 0, -1 },
    E = { 1, 0 },
    S = { 0, 1 },
    W = { -1, 0 }
}

local fromPoint = {
    [0] = { [1] = "S", [-1] = "N" },
    [1] = { [0] = "E" },
    [-1] = { [0] = "W" }
}

local function dir_from_point(p)
    return fromPoint[p[1]][p[2]]
end

local function dir_next(d)
    if d == "N" then return "E"
    elseif d == "E" then return "S"
    elseif d == "S" then return "W"
    else return "N" end
end

local function dir_prev(d)
    if d == "N" then return "W"
    elseif d == "W" then return "S"
    elseif d == "S" then return "E"
    else return "N" end
end

local fromByte = {
    ["N"] = "N", ["E"] = "E", ["S"] = "S", ["W"] = "W",
    ["U"] = "N", ["R"] = "E", ["D"] = "S", ["L"] = "W",
    ["^"] = "N", [">"] = "E", ["v"] = "S", ["<"] = "W"
}

local function dir_from_byte(b)
    return fromByte[b]
end

local function parse(program)
    local output = ""
    local in_func = function() end
    local out_func = function(val) output = output .. string.char(val) end
    run_program(program, in_func, out_func)
    local scaffolding = {}
    local robot = {}
    local dir
    local y = 0
    for line in output:gmatch("[^\n]+") do
        for x = 0, #line - 1 do
            local char = line:sub(x + 1, x + 1)
            local p = { x, y }
            if char == "^" or char == "v" or char == "<" or char == ">" then
                robot = p
                dir = dir_from_byte(char)
                scaffolding[y * 1000 + x] = true
            elseif char == "#" then
                scaffolding[y * 1000 + x] = true
            end
        end
        y = y + 1
    end
    return scaffolding, robot, dir
end

local function add_points(p1, p2)
    return { p1[1] + p2[1], p1[2] + p2[2] }
end

local function path(scaffolding, robot, dir)
    local dist = 0
    local d = ""
    local sections = {}
    while true do
        local next_point = add_points(robot, point[dir])
        if scaffolding[next_point[2] * 1000 + next_point[1]] then
            robot = next_point
            dist = dist + 1
        else
            if dist > 0 then
                table.insert(sections, d .. "," .. dist)
            end
            local next_dir = dir_next(dir)
            local next_point = add_points(robot, point[next_dir])
            if scaffolding[next_point[2] * 1000 + next_point[1]] then
                robot = next_point
                dir = next_dir
                dist = 1
                d = "R"
            else
                local prev_dir = dir_prev(dir)
                local prev_point = add_points(robot, point[prev_dir])
                if scaffolding[prev_point[2] * 1000 + prev_point[1]] then
                    robot = prev_point
                    dir = prev_dir
                    dist = 1
                    d = "L"
                else
                    break
                end
            end
        end
    end
    return table.concat(sections, ",")
end

local function encode(path)
    for i = 2, 21 do
        for j = 2, 21 do
            for k = 2, 21 do
                local next = path .. ","
                local a = next:sub(1, i)
                next = next:gsub(a, "")
                local b = next:sub(1, j)
                next = next:gsub(b, "")
                local c = next:sub(1, k)
                next = next:gsub(c, "")
                if next == "" then
                    a = a:sub(1, #a - 1)
                    b = b:sub(1, #b - 1)
                    c = c:sub(1, #c - 1)
                    local seq = path:gsub(a, "A")
                    seq = seq:gsub(b, "B")
                    seq = seq:gsub(c, "C")
                    seq = seq:sub(1, #seq)
                    return seq, a, b, c
                end
            end
        end
    end
end

local function dust(program, scaffolding, robot, dir)
    local seq, a, b, c = encode(path(scaffolding, robot, dir))
    local input = seq .. "\n" .. a .. "\n" .. b .. "\n" .. c .. "\n" .. "n\n"
    local input_index = 1
    local in_func = function()
        local char = input:sub(input_index, input_index)
        input_index = input_index + 1
        return string.byte(char)
    end
    local last_output = 0
    local out_func = function(val) last_output = val end
    program[1] = 2
    run_program(program, in_func, out_func)
    return last_output
end

local input = read_file("input.txt")
local program = parse_program(input)
local scaffolding, robot, dir = parse(program)
print(dust(program, scaffolding, robot, dir))
