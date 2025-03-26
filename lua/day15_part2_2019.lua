
--[[
    Day 15: Oxygen System
    Lua solution implementing Intcode computer and search algorithms.
]]

-- Utility function for creating coordinate keys
local function coordKey(x, y)
    return x .. "," .. y
end

-- Intcode Computer Implementation
local Intcode = {}
Intcode.__index = Intcode

function Intcode:new(program_str)
    local self = setmetatable({}, Intcode)
    self.memory = {}
    local i = 0
    for num_str in string.gmatch(program_str, "[^,]+") do
        self.memory[i] = tonumber(num_str)
        i = i + 1
    end
    self.ip = 0
    self.relativeBase = 0
    self.inputs = {}
    self.outputs = {}
    self.halted = false
    self.waitingForInput = false
    return self
end

function Intcode:getMem(addr)
    if addr < 0 then error("Invalid memory address: " .. addr) end
    return self.memory[addr] or 0
end

function Intcode:setMem(addr, value)
    if addr < 0 then error("Invalid memory address: " .. addr) end
    self.memory[addr] = value
end

function Intcode:getParam(offset, mode)
    local paramAddr = self.ip + offset
    local paramVal = self:getMem(paramAddr)
    if mode == 0 then -- Position mode
        return self:getMem(paramVal)
    elseif mode == 1 then -- Immediate mode
        return paramVal
    elseif mode == 2 then -- Relative mode
        return self:getMem(self.relativeBase + paramVal)
    else
        error("Invalid parameter mode: " .. mode)
    end
end

function Intcode:setParam(offset, mode, value)
    local paramAddr = self.ip + offset
    local paramVal = self:getMem(paramAddr)
    if mode == 0 then -- Position mode
        self:setMem(paramVal, value)
    elseif mode == 2 then -- Relative mode
        self:setMem(self.relativeBase + paramVal, value)
    else
        error("Invalid target parameter mode: " .. mode)
    end
end

function Intcode:addInput(value)
    table.insert(self.inputs, value)
    self.waitingForInput = false -- No longer waiting after receiving input
end

function Intcode:getOutput()
    if #self.outputs > 0 then
        return table.remove(self.outputs, 1)
    end
    return nil
end

function Intcode:run()
    self.waitingForInput = false
    while not self.halted and not self.waitingForInput do
        local instruction = self:getMem(self.ip)
        local opcode = instruction % 100
        local mode1 = math.floor(instruction / 100) % 10
        local mode2 = math.floor(instruction / 1000) % 10
        local mode3 = math.floor(instruction / 10000) % 10

        -- print(string.format("IP: %d, INST: %d, OP: %d, M: %d%d%d, RB: %d", self.ip, instruction, opcode, mode3,mode2,mode1, self.relativeBase))

        if opcode == 1 then -- Add
            local val1 = self:getParam(1, mode1)
            local val2 = self:getParam(2, mode2)
            self:setParam(3, mode3, val1 + val2)
            self.ip = self.ip + 4
        elseif opcode == 2 then -- Multiply
            local val1 = self:getParam(1, mode1)
            local val2 = self:getParam(2, mode2)
            self:setParam(3, mode3, val1 * val2)
            self.ip = self.ip + 4
        elseif opcode == 3 then -- Input
            if #self.inputs > 0 then
                local inputVal = table.remove(self.inputs, 1)
                self:setParam(1, mode1, inputVal)
                self.ip = self.ip + 2
            else
                self.waitingForInput = true -- Wait for more input
                -- Do not advance IP, will retry opcode 3 next run
            end
        elseif opcode == 4 then -- Output
            local outputVal = self:getParam(1, mode1)
            table.insert(self.outputs, outputVal)
            self.ip = self.ip + 2
            -- If controlling a droid, might want to pause/return after output
            return "output" -- Signal that output was produced
        elseif opcode == 5 then -- Jump-if-true
            local val1 = self:getParam(1, mode1)
            local val2 = self:getParam(2, mode2)
            if val1 ~= 0 then
                self.ip = val2
            else
                self.ip = self.ip + 3
            end
        elseif opcode == 6 then -- Jump-if-false
            local val1 = self:getParam(1, mode1)
            local val2 = self:getParam(2, mode2)
            if val1 == 0 then
                self.ip = val2
            else
                self.ip = self.ip + 3
            end
        elseif opcode == 7 then -- Less than
            local val1 = self:getParam(1, mode1)
            local val2 = self:getParam(2, mode2)
            self:setParam(3, mode3, (val1 < val2) and 1 or 0)
            self.ip = self.ip + 4
        elseif opcode == 8 then -- Equals
            local val1 = self:getParam(1, mode1)
            local val2 = self:getParam(2, mode2)
            self:setParam(3, mode3, (val1 == val2) and 1 or 0)
            self.ip = self.ip + 4
        elseif opcode == 9 then -- Adjust relative base
            local val1 = self:getParam(1, mode1)
            self.relativeBase = self.relativeBase + val1
            self.ip = self.ip + 2
        elseif opcode == 99 then -- Halt
            self.halted = true
        else
            error("Unknown opcode: " .. opcode .. " at ip " .. self.ip)
        end
    end
    if self.halted then return "halted" end
    if self.waitingForInput then return "waiting" end
    return "ok" -- Continue running normally if possible
end

-- --- Main Logic ---

-- Map exploration using Depth First Search (DFS)
-- We use a single droid and backtrack
local function exploreMap(computer, grid, x, y, oxygenPos)
    local moves = { [1] = {0, -1}, [2] = {0, 1}, [3] = {-1, 0}, [4] = {1, 0} } -- N, S, W, E
    local oppositeMove = { [1] = 2, [2] = 1, [3] = 4, [4] = 3 }

    for moveCmd = 1, 4 do
        local dx, dy = moves[moveCmd][1], moves[moveCmd][2]
        local nx, ny = x + dx, y + dy
        local nk = coordKey(nx, ny)

        if grid[nk] == nil then -- Only explore unknown locations
            computer:addInput(moveCmd)
            computer:run() -- Should produce one output
            local status = computer:getOutput()

            if status == nil then error("Expected output from computer, got none.") end

            if status == 0 then -- Hit a wall
                grid[nk] = '#'
            elseif status == 1 or status == 2 then -- Moved
                grid[nk] = '.' -- Mark as open path
                if status == 2 then
                    grid[nk] = 'O' -- Mark as oxygen system
                    oxygenPos.x = nx
                    oxygenPos.y = ny
                    -- Don't stop exploring, need the full map for Part 2
                end

                -- Recursively explore from the new position
                exploreMap(computer, grid, nx, ny, oxygenPos)

                -- Backtrack: Move the droid back to the previous position (x, y)
                local backMove = oppositeMove[moveCmd]
                computer:addInput(backMove)
                computer:run() -- Should produce output 1 (successful move back)
                local backStatus = computer:getOutput()
                if backStatus ~= 1 and backStatus ~= 2 then -- Should always be able to move back
                     error(string.format("Failed to backtrack from (%d,%d) to (%d,%d) with move %d, status %d", nx, ny, x, y, backMove, backStatus or -1))
                end
            else
                error("Unknown status code: " .. status)
            end
        end
    end
end

-- Breadth First Search to find the shortest path on the known grid
local function findShortestPath(grid, startX, startY, targetX, targetY)
    local queue = {}
    local visited = {}
    local moves = { {0, -1}, {0, 1}, {-1, 0}, {1, 0} } -- N, S, W, E (relative changes)

    table.insert(queue, {x = startX, y = startY, dist = 0})
    visited[coordKey(startX, startY)] = true

    local head = 1
    while head <= #queue do
        local current = queue[head]
        head = head + 1

        if current.x == targetX and current.y == targetY then
            return current.dist
        end

        for _, move in ipairs(moves) do
            local nx, ny = current.x + move[1], current.y + move[2]
            local nk = coordKey(nx, ny)

            if grid[nk] and grid[nk] ~= '#' and not visited[nk] then
                visited[nk] = true
                table.insert(queue, {x = nx, y = ny, dist = current.dist + 1})
            end
        end
    end

    return -1 -- Should not happen if target is reachable
end

-- Breadth First Search to calculate oxygen fill time
local function calculateFillTime(grid, startX, startY)
    local queue = {}
    local visited = {}
    local moves = { {0, -1}, {0, 1}, {-1, 0}, {1, 0} } -- N, S, W, E (relative changes)
    local maxTime = 0

    table.insert(queue, {x = startX, y = startY, time = 0})
    visited[coordKey(startX, startY)] = true

    local head = 1
    while head <= #queue do
        local current = queue[head]
        head = head + 1

        maxTime = math.max(maxTime, current.time)

        for _, move in ipairs(moves) do
            local nx, ny = current.x + move[1], current.y + move[2]
            local nk = coordKey(nx, ny)

            -- Check if neighbor is an open space ('.') and not visited
            if grid[nk] == '.' and not visited[nk] then
                visited[nk] = true
                -- Note: Even if grid[nk] was 'O' initially (the start),
                -- we only add '.' neighbors to the queue.
                table.insert(queue, {x = nx, y = ny, time = current.time + 1})
            end
        end
    end

    return maxTime
end


-- Main execution function
local function main()
    -- Read program from input.txt
    local file = io.open("input.txt", "r")
    if not file then
        print("Error: Could not open input.txt")
        return
    end
    local program_str = file:read("*a")
    file:close()
    program_str = program_str:match("^%s*(.-)%s*$") -- Trim whitespace

    -- Initialize Intcode computer
    local computer = Intcode:new(program_str)

    -- Initialize grid and starting position
    local grid = {}
    local startX, startY = 0, 0
    grid[coordKey(startX, startY)] = '.' -- Start position is open
    local oxygenPos = {x = nil, y = nil} -- To store oxygen system location

    -- Explore the entire map using DFS
    exploreMap(computer, grid, startX, startY, oxygenPos)

    if not oxygenPos.x then
        print("Error: Oxygen system not found during exploration.")
        return
    end

    -- Part 1: Find shortest path from start to oxygen system
    local shortestPath = findShortestPath(grid, startX, startY, oxygenPos.x, oxygenPos.y)
    print("Part 1: Fewest movements to oxygen system: " .. shortestPath)

    -- Part 2: Calculate time to fill with oxygen
    -- Important: The BFS starts from the oxygen system ('O'), but only spreads to '.' cells.
    -- The starting 'O' cell itself takes 0 time.
    grid[coordKey(oxygenPos.x, oxygenPos.y)] = '.' -- Treat the oxygen source as spreadable for BFS logic
    local fillTime = calculateFillTime(grid, oxygenPos.x, oxygenPos.y)
    print("Part 2: Time to fill with oxygen: " .. fillTime)

end

-- Run the main function
main()
