-- Read input from file
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local content = file:read("*all")
    file:close()
    local program = {}
    for num in content:gmatch("[-]?%d+") do
        table.insert(program, tonumber(num))
    end
    return program
end

-- Intcode computer
local function intcode_computer(program)
    local memory = {}
    for i, v in ipairs(program) do memory[i-1] = v end
    local pc, relative_base = 0, 0

    local function get_param(mode, value)
        if mode == 0 then return memory[value] or 0
        elseif mode == 1 then return value
        elseif mode == 2 then return memory[relative_base + value] or 0
        else error("Unknown parameter mode: " .. mode) end
    end

    local function set_param(mode, addr, value)
        if mode == 0 then memory[addr] = value
        elseif mode == 2 then memory[relative_base + addr] = value
        else error("Invalid mode for setting value: " .. mode) end
    end

    return function(input)
        while true do
            local instruction = memory[pc]
            local opcode = instruction % 100
            local mode1 = math.floor(instruction / 100) % 10
            local mode2 = math.floor(instruction / 1000) % 10
            local mode3 = math.floor(instruction / 10000) % 10

            if opcode == 99 then return nil end

            if opcode == 1 then -- add
                local p1, p2 = get_param(mode1, memory[pc+1]), get_param(mode2, memory[pc+2])
                set_param(mode3, memory[pc+3], p1 + p2)
                pc = pc + 4
            elseif opcode == 2 then -- multiply
                local p1, p2 = get_param(mode1, memory[pc+1]), get_param(mode2, memory[pc+2])
                set_param(mode3, memory[pc+3], p1 * p2)
                pc = pc + 4
            elseif opcode == 3 then -- input
                set_param(mode1, memory[pc+1], input())
                pc = pc + 2
            elseif opcode == 4 then -- output
                local output = get_param(mode1, memory[pc+1])
                pc = pc + 2
                return output
            elseif opcode == 5 then -- jump-if-true
                local p1, p2 = get_param(mode1, memory[pc+1]), get_param(mode2, memory[pc+2])
                pc = p1 ~= 0 and p2 or pc + 3
            elseif opcode == 6 then -- jump-if-false
                local p1, p2 = get_param(mode1, memory[pc+1]), get_param(mode2, memory[pc+2])
                pc = p1 == 0 and p2 or pc + 3
            elseif opcode == 7 then -- less than
                local p1, p2 = get_param(mode1, memory[pc+1]), get_param(mode2, memory[pc+2])
                set_param(mode3, memory[pc+3], p1 < p2 and 1 or 0)
                pc = pc + 4
            elseif opcode == 8 then -- equals
                local p1, p2 = get_param(mode1, memory[pc+1]), get_param(mode2, memory[pc+2])
                set_param(mode3, memory[pc+3], p1 == p2 and 1 or 0)
                pc = pc + 4
            elseif opcode == 9 then -- adjust relative base
                relative_base = relative_base + get_param(mode1, memory[pc+1])
                pc = pc + 2
            else
                error("Unknown opcode: " .. opcode)
            end
        end
    end
end

-- Robot function
local function run_robot(program)
    local computer = intcode_computer(program)
    local panels = {}
    local x, y = 0, 0
    local dx, dy = 0, -1  -- Start facing up
    local painted_panels = 0

    local function get_color()
        return panels[y .. "," .. x] or 0
    end

    local function paint(color)
        if not panels[y .. "," .. x] then
            painted_panels = painted_panels + 1
        end
        panels[y .. "," .. x] = color
    end

    local function turn_and_move(direction)
        if direction == 0 then  -- Turn left
            dx, dy = dy, -dx
        else  -- Turn right
            dx, dy = -dy, dx
        end
        x, y = x + dx, y + dy
    end

    while true do
        local color = computer(function() return get_color() end)
        if not color then break end
        paint(color)
        
        local direction = computer(function() return get_color() end)
        if not direction then break end
        turn_and_move(direction)
    end

    return painted_panels
end

-- Robot function
local function run_robot(program)
    local computer = intcode_computer(program)
    local panels = {["0,0"] = 1}  -- Start on a white panel
    local x, y = 0, 0
    local dx, dy = 0, -1  -- Start facing up
    local min_x, max_x, min_y, max_y = 0, 0, 0, 0

    local function get_color()
        return panels[y .. "," .. x] or 0
    end

    local function paint(color)
        panels[y .. "," .. x] = color
        min_x, max_x = math.min(min_x, x), math.max(max_x, x)
        min_y, max_y = math.min(min_y, y), math.max(max_y, y)
    end

    local function turn_and_move(direction)
        if direction == 0 then  -- Turn left
            dx, dy = dy, -dx
        else  -- Turn right
            dx, dy = -dy, dx
        end
        x, y = x + dx, y + dy
    end

    while true do
        local color = computer(function() return get_color() end)
        if not color then break end
        paint(color)
        
        local direction = computer(function() return get_color() end)
        if not direction then break end
        turn_and_move(direction)
    end

    return panels, min_x, max_x, min_y, max_y
end

-- Visualize the painted panels
local function visualize_panels(panels, min_x, max_x, min_y, max_y)
    for y = min_y, max_y do
        local row = ""
        for x = min_x, max_x do
            row = row .. (panels[y .. "," .. x] == 1 and "█" or " ")
        end
        print(row)
    end
end

-- Main function
local function main()
    local program = read_input()
    local panels, min_x, max_x, min_y, max_y = run_robot(program)
    print("Registration identifier:")
    visualize_panels(panels, min_x, max_x, min_y, max_y)
end

main()
