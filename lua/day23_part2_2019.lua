
local M = {}

local memory_mt = {
    __index = function(t, k)
        return 0
    end
}

function M.new_intcode_computer(program_code, initial_inputs)
    local computer = {
        memory = setmetatable({}, memory_mt),
        ip = 0,
        relative_base = 0,
        inputs = initial_inputs or {},
        outputs = {},
        halted = false,
        idle = false,
        input_ptr = 1,
        output_ptr = 1
    }
    for i, code in ipairs(program_code) do
        computer.memory[i - 1] = code
    end
    return computer
end

function M.get_param(computer, mode, offset)
    local addr
    if mode == 0 then -- Position mode
        addr = computer.memory[computer.ip + offset]
    elseif mode == 1 then -- Immediate mode
        return computer.memory[computer.ip + offset]
    elseif mode == 2 then -- Relative mode
        addr = computer.relative_base + computer.memory[computer.ip + offset]
    else
        error("Unknown parameter mode: " .. mode)
    end
    return computer.memory[addr]
end

function M.set_param(computer, mode, offset, value)
    local addr
    if mode == 0 then -- Position mode
        addr = computer.memory[computer.ip + offset]
    elseif mode == 2 then -- Relative mode
        addr = computer.relative_base + computer.memory[computer.ip + offset]
    else
        error("Unknown parameter mode for writing: " .. mode)
    end
    computer.memory[addr] = value
end

function M.run_intcode(computer)
    computer.idle = false -- Assume not idle unless waiting for input
    while true do
        local instruction = computer.memory[computer.ip]
        local opcode = instruction % 100
        local modes = {
            math.floor(instruction / 100) % 10,
            math.floor(instruction / 1000) % 10,
            math.floor(instruction / 10000) % 10
        }

        if opcode == 99 then
            computer.halted = true
            return
        elseif opcode == 1 then -- add
            local p1 = M.get_param(computer, modes[1], 1)
            local p2 = M.get_param(computer, modes[2], 2)
            M.set_param(computer, modes[3], 3, p1 + p2)
            computer.ip = computer.ip + 4
        elseif opcode == 2 then -- multiply
            local p1 = M.get_param(computer, modes[1], 1)
            local p2 = M.get_param(computer, modes[2], 2)
            M.set_param(computer, modes[3], 3, p1 * p2)
            computer.ip = computer.ip + 4
        elseif opcode == 3 then -- input
            local input_value
            if computer.input_ptr > #computer.inputs then
                 input_value = -1
                 computer.idle = true
            else
                 input_value = computer.inputs[computer.input_ptr]
                 computer.input_ptr = computer.input_ptr + 1
            end
            M.set_param(computer, modes[1], 1, input_value)
            computer.ip = computer.ip + 2
            if computer.idle then
                 -- Clear remaining inputs if any? No, Python version keeps them. Reset ptr for next run.
                 computer.input_ptr = 1
                 computer.inputs = {}
                 return -- Pause execution, wait for more input
            end
        elseif opcode == 4 then -- output
            local p1 = M.get_param(computer, modes[1], 1)
            computer.outputs[#computer.outputs + 1] = p1
            computer.ip = computer.ip + 2
            if #computer.outputs - computer.output_ptr + 1 == 3 then
                 return -- Packet ready
            end
        elseif opcode == 5 then -- jump-if-true
            local p1 = M.get_param(computer, modes[1], 1)
            local p2 = M.get_param(computer, modes[2], 2)
            if p1 ~= 0 then
                computer.ip = p2
            else
                computer.ip = computer.ip + 3
            end
        elseif opcode == 6 then -- jump-if-false
            local p1 = M.get_param(computer, modes[1], 1)
            local p2 = M.get_param(computer, modes[2], 2)
            if p1 == 0 then
                computer.ip = p2
            else
                computer.ip = computer.ip + 3
            end
        elseif opcode == 7 then -- less than
            local p1 = M.get_param(computer, modes[1], 1)
            local p2 = M.get_param(computer, modes[2], 2)
            M.set_param(computer, modes[3], 3, (p1 < p2 and 1 or 0))
            computer.ip = computer.ip + 4
        elseif opcode == 8 then -- equals
            local p1 = M.get_param(computer, modes[1], 1)
            local p2 = M.get_param(computer, modes[2], 2)
            M.set_param(computer, modes[3], 3, (p1 == p2 and 1 or 0))
            computer.ip = computer.ip + 4
        elseif opcode == 9 then -- adjust relative base
            local p1 = M.get_param(computer, modes[1], 1)
            computer.relative_base = computer.relative_base + p1
            computer.ip = computer.ip + 2
        else
            error("Unknown opcode: " .. opcode .. " at ip " .. computer.ip)
        end
    end
end

local function read_program(filename)
    local file = io.open(filename, "r")
    if not file then error("Cannot open input file: " .. filename) end
    local content = file:read("*a")
    file:close()
    local program = {}
    for num_str in string.gmatch(content, "[^,%s]+") do
        program[#program + 1] = tonumber(num_str)
    end
    return program
end


local function main()
    local program = read_program("input.txt")

    local computers = {}
    local packet_queues = {}

    for i = 0, 49 do
        computers[i] = M.new_intcode_computer(program, {i})
        packet_queues[i] = {}
    end

    local nat_packet = nil
    local prev_nat_y = nil

    while true do
        local network_potentially_idle = true

        for i = 0, 49 do
            local computer = computers[i]

            -- Prepare input: Provide queued packets or -1
            if #packet_queues[i] > 0 then
                local packet = table.remove(packet_queues[i], 1)
                computer.inputs[#computer.inputs + 1] = packet[1]
                computer.inputs[#computer.inputs + 1] = packet[2]
                network_potentially_idle = false -- Activity due to packet queue
            else
                 -- Only add -1 if the computer doesn't already have pending input
                 if computer.input_ptr > #computer.inputs then
                     computer.inputs[#computer.inputs + 1] = -1
                 else
                      -- Computer still processing previous inputs, not idle yet
                      network_potentially_idle = false
                 end
            end

            M.run_intcode(computer)

            -- Process outputs
            while #computer.outputs - computer.output_ptr + 1 >= 3 do
                network_potentially_idle = false -- Activity due to output
                local dest = computer.outputs[computer.output_ptr]
                local x = computer.outputs[computer.output_ptr + 1]
                local y = computer.outputs[computer.output_ptr + 2]
                computer.output_ptr = computer.output_ptr + 3

                if dest == 255 then
                    nat_packet = {x, y}
                elseif dest >= 0 and dest < 50 then
                    table.insert(packet_queues[dest], {x, y})
                end
            end
             -- Clean up processed outputs to avoid memory leak
             if computer.output_ptr > 1 and computer.output_ptr -1 >= #computer.outputs then
                 computer.outputs = {}
                 computer.output_ptr = 1
             end


             -- Check if this specific computer became idle
             if not computer.idle then
                 network_potentially_idle = false
             end
        end

        -- Check if the entire network is idle
        local all_idle = true
        for i = 0, 49 do
             if #packet_queues[i] > 0 or not computers[i].idle then
                all_idle = false
                break
             end
        end


        if all_idle and nat_packet then
             local x = nat_packet[1]
             local y = nat_packet[2]
             table.insert(packet_queues[0], {x, y})

             if prev_nat_y ~= nil and y == prev_nat_y then
                print(y)
                return -- Exit main
             end
             prev_nat_y = y
             -- nat_packet = nil -- Let it be overwritten next time 255 receives a packet
        end
    end
end

main()
