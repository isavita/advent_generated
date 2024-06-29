
local function read_input()
    local floor = {}
    for line in io.lines("input.txt") do
        local items = {}
        for item in line:gmatch("%w+[-%w]+ %w+") do
            table.insert(items, item)
        end
        table.insert(floor, items)
    end
    return floor
end

local function is_valid_state(state)
    for _, floor in ipairs(state) do
        local generators = {}
        local microchips = {}
        for _, item in ipairs(floor) do
            if item:match("generator") then
                generators[item:match("(%w+)")] = true
            else
                microchips[item:match("(%w+)")] = true
            end
        end
        for chip, _ in pairs(microchips) do
            if next(generators) and not generators[chip] then
                return false
            end
        end
    end
    return true
end

local function generate_moves(state, elevator)
    local moves = {}
    local current_floor = state[elevator]
    
    for i = 1, #current_floor do
        for j = i, #current_floor do
            for dir = -1, 1, 2 do
                local new_elevator = elevator + dir
                if new_elevator >= 1 and new_elevator <= 4 then
                    local new_state = {table.unpack(state)}
                    new_state[elevator] = {table.unpack(current_floor)}
                    new_state[new_elevator] = {table.unpack(state[new_elevator])}
                    
                    table.insert(new_state[new_elevator], table.remove(new_state[elevator], i))
                    if i ~= j then
                        table.insert(new_state[new_elevator], table.remove(new_state[elevator], j - 1))
                    end
                    
                    if is_valid_state(new_state) then
                        table.insert(moves, {state = new_state, elevator = new_elevator})
                    end
                end
            end
        end
    end
    
    return moves
end

local function is_goal_state(state)
    return #state[4] == #state[1] + #state[2] + #state[3] + #state[4]
end

local function solve(initial_state)
    local queue = {{state = initial_state, elevator = 1, steps = 0}}
    local visited = {}
    
    while #queue > 0 do
        local current = table.remove(queue, 1)
        local state_key = table.concat(current.state[1]) .. "|" .. 
                          table.concat(current.state[2]) .. "|" .. 
                          table.concat(current.state[3]) .. "|" .. 
                          table.concat(current.state[4]) .. "|" .. 
                          current.elevator
        
        if not visited[state_key] then
            visited[state_key] = true
            
            if is_goal_state(current.state) then
                return current.steps
            end
            
            local moves = generate_moves(current.state, current.elevator)
            for _, move in ipairs(moves) do
                table.insert(queue, {state = move.state, elevator = move.elevator, steps = current.steps + 1})
            end
        end
    end
    
    return -1  -- No solution found
end

local initial_state = read_input()
local min_steps = solve(initial_state)
print("Minimum number of steps required:", min_steps)
