
local BROADCASTER = 0
local FLIP_FLOP = 1
local CONJUNCTION = 2

local function split(str, sep)
    local parts = {}
    local pattern = string.format("([^%s]+)", sep)
    str:gsub(pattern, function(c) parts[#parts+1] = c:match'^%s*(.-)%s*$' end)
    return parts
end

local function handle_line(line, connections)
    local parts = split(line, "->")
    local definition = parts[1]
    local targets_str = parts[2]
    local targets = split(targets_str, ",")

    local module_type
    local name
    local module

    if definition == "broadcaster" then
        module_type = BROADCASTER
        name = definition
        module = { name = name, module_type = module_type, connects_to = targets }
    elseif definition:sub(1, 1) == "%" then
        module_type = FLIP_FLOP
        name = definition:sub(2)
        module = { name = name, module_type = module_type, connects_to = targets, state = false }
    else -- Assuming '&'
        module_type = CONJUNCTION
        name = definition:sub(2)
        module = { name = name, module_type = module_type, connects_to = targets, watches = {} }
    end
    connections[name] = module
end

local function complete_watches(connections)
    for name, module in pairs(connections) do
        if module.module_type == CONJUNCTION then
            for name2, module2 in pairs(connections) do
                for _, target_name in ipairs(module2.connects_to) do
                    if target_name == name then
                        module.watches[name2] = false
                    end
                end
            end
        end
    end
end

local function all_true(tbl)
    for _, v in pairs(tbl) do
        if not v then
            return false
        end
    end
    return true
end

local function simulate_press(connections, loops, press_number)
    local queue = {}
    table.insert(queue, { from = "button", name = "broadcaster", pulse = false })
    -- pulses[1] = low, pulses[2] = high. Initial button press is low.
    local pulses = {1, 0}
    local found_rx_low = false

    local head = 1
    while head <= #queue do
        local curr_state = queue[head]
        head = head + 1

        local module = connections[curr_state.name]

        if not module then
            goto continue -- Skip if module doesn't exist (like 'output' or 'rx' if not defined)
        end

        if curr_state.name == "rx" and not curr_state.pulse then
            found_rx_low = true
            -- Don't stop simulation here, let it finish the current press
        end

        local pulse_to_send = nil

        if module.module_type == BROADCASTER then
            pulse_to_send = curr_state.pulse
        elseif module.module_type == FLIP_FLOP then
            if not curr_state.pulse then -- Only reacts to low pulse
                module.state = not module.state
                pulse_to_send = module.state
            end
            -- If high pulse, do nothing, pulse_to_send remains nil
        elseif module.module_type == CONJUNCTION then
            module.watches[curr_state.from] = curr_state.pulse
            pulse_to_send = not all_true(module.watches)

            if loops and loops[curr_state.name] and pulse_to_send and loops[curr_state.name] == -1 then
                 loops[curr_state.name] = press_number
            end
        end

        if pulse_to_send ~= nil then
            for _, target_name in ipairs(module.connects_to) do
                table.insert(queue, { from = module.name, name = target_name, pulse = pulse_to_send })
                if pulse_to_send then
                    pulses[2] = pulses[2] + 1
                else
                    pulses[1] = pulses[1] + 1
                end
            end
        end

        ::continue::
    end

    return pulses, found_rx_low
end

local function connects_to(frm_name, to_name, connections)
    local module = connections[frm_name]
    if not module then return false end
    for _, target in ipairs(module.connects_to) do
        if target == to_name then
            return true
        end
    end
    return false
end

local function all_loops_found(tbl)
     if not tbl then return false end
     for _, v in pairs(tbl) do
         if v == -1 then
             return false
         end
     end
     return true
end

local function gcd(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

local function lcm(a, b)
    if a == 0 or b == 0 then return 0 end
    return math.abs(a * b) // gcd(a, b) -- Use // for integer division if needed, Lua numbers are floats
end


local function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening input.txt")
        return
    end

    local connections = {}
    for line in file:lines() do
        local trimmed_line = line:match'^%s*(.-)%s*$'
        if #trimmed_line > 0 then
            handle_line(trimmed_line, connections)
        end
    end
    file:close()

    complete_watches(connections)

    local px_prev_name = nil
    for name, _ in pairs(connections) do
        if connects_to(name, "rx", connections) then
            if px_prev_name then
                 -- This problem variant assumes only one input to rx
                 print("Error: more than one input to rx found")
                 return
            end
            px_prev_name = name
        end
    end

    if not px_prev_name then
        -- If rx or its predecessor isn't in the graph, part 2 logic doesn't apply
        -- Let's assume part 1 might be needed. Calculate pulses for 1000 presses.
        local total_pulses = {0, 0}
         for i = 1, 1000 do
            local p, _ = simulate_press(connections, nil, i)
            total_pulses[1] = total_pulses[1] + p[1]
            total_pulses[2] = total_pulses[2] + p[2]
        end
        print(total_pulses[1] * total_pulses[2])
        return
    end


    local conj = connections[px_prev_name]
    if not conj or conj.module_type ~= CONJUNCTION then
         print("Error: Predecessor to rx is not a conjunction module")
         return
    end

    local loop_lengths = {}
    for input_name, _ in pairs(conj.watches) do
        loop_lengths[input_name] = -1
    end

    local press_number = 0
    local result = 1

    while true do
        press_number = press_number + 1
        local _, found = simulate_press(connections, loop_lengths, press_number)

        if found then
            -- This case corresponds to Part 2 finding the exact press number
            -- but the puzzle usually relies on LCM of cycles.
            -- If 'found' is the primary goal, print press_number.
            -- print(press_number)
            -- break
            -- However, the python code calculates LCM, so we stick to that.
            -- If found becomes true, it means rx got a low pulse.
        end

        if all_loops_found(loop_lengths) then
             result = 1
             for _, length in pairs(loop_lengths) do
                 result = lcm(result, length)
             end
             break
        end

        -- Safety break for unexpected inputs
        if press_number > 100000 then -- Adjust limit if necessary
            print("Simulation exceeded limit without finding all loops.")
            result = -1 -- Indicate failure or unexpected state
            break
        end
    end

    print(result)

end

main()
