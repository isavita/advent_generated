
local function split(inputstr, sep)
    sep = sep or ','
    local t = {}
    for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
        table.insert(t, tonumber(str))
    end
    return t
end

local function readFile(filename)
    local file = io.open(filename, "r")
    local content = file:read("*all"):gsub("%s+", "")
    file:close()
    return content
end

local MODE = {
    POSITION = 0,
    IMMEDIATE = 1,
    RELATIVE = 2
}

local OPCODE = {
    ADD = 1,
    MUL = 2,
    INPUT = 3,
    OUTPUT = 4,
    JT = 5,
    JF = 6,
    LT = 7,
    EQ = 8,
    RBO = 9,
    HALT = 99
}

local function decode(n)
    local op = n % 100
    n = math.floor(n / 100)
    local modes = {0, 0, 0}
    for i = 1, 3 do
        modes[i] = n % 10
        n = math.floor(n / 10)
    end
    return op, modes
end

local Machine = {}
Machine.__index = Machine

function Machine.new(program, input_fn, output_fn)
    local self = setmetatable({}, Machine)
    self.data = {}
    self.ip = 1
    self.relbase = 1
    self.input_fn = input_fn
    self.output_fn = output_fn

    for i, v in ipairs(program) do
        self.data[i] = v
    end

    return self
end

function Machine:get(i, mode)
    mode = mode or MODE.POSITION
    if mode == MODE.IMMEDIATE then
        return self.data[i] or 0
    elseif mode == MODE.POSITION then
        return self.data[self.data[i] or 0] or 0
    elseif mode == MODE.RELATIVE then
        return self.data[self.relbase + (self.data[i] or 0)] or 0
    end
end

function Machine:set(i, mode, val)
    mode = mode or MODE.POSITION
    if mode == MODE.POSITION then
        self.data[self.data[i]] = val
    elseif mode == MODE.RELATIVE then
        self.data[self.relbase + self.data[i]] = val
    end
end

function Machine:step()
    local op, modes = decode(self.data[self.ip])
    
    if op == OPCODE.ADD then
        local val = self:get(self.ip+1, modes[1]) + self:get(self.ip+2, modes[2])
        self:set(self.ip+3, modes[3], val)
        self.ip = self.ip + 4
    elseif op == OPCODE.MUL then
        local val = self:get(self.ip+1, modes[1]) * self:get(self.ip+2, modes[2])
        self:set(self.ip+3, modes[3], val)
        self.ip = self.ip + 4
    elseif op == OPCODE.INPUT then
        self:set(self.ip+1, modes[1], self.input_fn())
        self.ip = self.ip + 2
    elseif op == OPCODE.OUTPUT then
        self.output_fn(self:get(self.ip+1, modes[1]))
        self.ip = self.ip + 2
    elseif op == OPCODE.JT then
        if self:get(self.ip+1, modes[1]) ~= 0 then
            self.ip = self:get(self.ip+2, modes[2])
        else
            self.ip = self.ip + 3
        end
    elseif op == OPCODE.JF then
        if self:get(self.ip+1, modes[1]) == 0 then
            self.ip = self:get(self.ip+2, modes[2])
        else
            self.ip = self.ip + 3
        end
    elseif op == OPCODE.LT then
        local val = self:get(self.ip+1, modes[1]) < self:get(self.ip+2, modes[2]) and 1 or 0
        self:set(self.ip+3, modes[3], val)
        self.ip = self.ip + 4
    elseif op == OPCODE.EQ then
        local val = self:get(self.ip+1, modes[1]) == self:get(self.ip+2, modes[2]) and 1 or 0
        self:set(self.ip+3, modes[3], val)
        self.ip = self.ip + 4
    elseif op == OPCODE.RBO then
        self.relbase = self.relbase + self:get(self.ip+1, modes[1])
        self.ip = self.ip + 2
    elseif op == OPCODE.HALT then
        return false
    end
    
    return true
end

function Machine:run()
    while self:step() do end
end

local function manhattan(p1, p2)
    return math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)
end

local Pathfinder = {}
Pathfinder.__index = Pathfinder

function Pathfinder.new(program)
    local self = setmetatable({}, Pathfinder)
    self.grid = {}
    self.pos = {x = 0, y = 0}
    self.oxygen = nil
    
    local input_queue = {}
    local output_queue = {}
    
    local function input_fn()
        return table.remove(input_queue, 1)
    end
    
    local function output_fn(val)
        table.insert(output_queue, val)
    end
    
    self.machine = Machine.new(program, input_fn, output_fn)
    self.input_queue = input_queue
    self.output_queue = output_queue
    
    self.directions = {
        {x = 0, y = 1, code = 1},   -- North
        {x = 0, y = -1, code = 2},  -- South
        {x = -1, y = 0, code = 3},  -- West
        {x = 1, y = 0, code = 4}    -- East
    }
    
    self.grid[self.pos.x .. ',' .. self.pos.y] = '.'
    
    return self
end

function Pathfinder:try_move(dir)
    table.insert(self.input_queue, dir.code)
    self.machine:step()
    local result = table.remove(self.output_queue, 1)
    
    local next_pos = {x = self.pos.x + dir.x, y = self.pos.y + dir.y}
    local key = next_pos.x .. ',' .. next_pos.y
    
    if result == 0 then
        self.grid[key] = '#'
        return false
    elseif result == 1 then
        self.grid[key] = '.'
        self.pos = next_pos
        return true
    elseif result == 2 then
        self.grid[key] = 'O'
        self.pos = next_pos
        self.oxygen = next_pos
        return true
    end
end

function Pathfinder:explore()
    local visited = {}
    local function is_unexplored(pos)
        local key = pos.x .. ',' .. pos.y
        return not visited[key] and not self.grid[key]
    end
    
    while true do
        local found_move = false
        for _, dir in ipairs(self.directions) do
            local next_pos = {x = self.pos.x + dir.x, y = self.pos.y + dir.y}
            local key = next_pos.x .. ',' .. next_pos.y
            
            if is_unexplored(next_pos) then
                if self:try_move(dir) then
                    visited[key] = true
                    found_move = true
                    break
                end
            end
        end
        
        if not found_move then break end
    end
end

function Pathfinder:shortest_path(start, goal)
    local queue = {{pos = start, path = {}}}
    local visited = {}
    
    while #queue > 0 do
        local current = table.remove(queue, 1)
        local key = current.pos.x .. ',' .. current.pos.y
        
        if current.pos.x == goal.x and current.pos.y == goal.y then
            return current.path
        end
        
        if not visited[key] then
            visited[key] = true
            
            for _, dir in ipairs(self.directions) do
                local next_pos = {x = current.pos.x + dir.x, y = current.pos.y + dir.y}
                local next_key = next_pos.x .. ',' .. next_pos.y
                
                if self.grid[next_key] ~= '#' and not visited[next_key] then
                    local new_path = {}
                    for _, p in ipairs(current.path) do
                        table.insert(new_path, p)
                    end
                    table.insert(new_path, dir)
                    
                    table.insert(queue, {pos = next_pos, path = new_path})
                end
            end
        end
    end
end

local function main()
    local input = readFile('input.txt')
    local program = split(input)
    
    local pathfinder = Pathfinder.new(program)
    pathfinder:explore()
    
    local path = pathfinder:shortest_path({x = 0, y = 0}, pathfinder.oxygen)
    print(#path)
end

main()
