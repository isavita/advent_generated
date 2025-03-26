
--[[
    Advent of Code 2019 - Day 7: Amplification Circuit
    Lua solution implementing both parts of the challenge.
]]

-- Utility function for splitting a string by a delimiter
local function split(str, sep)
    local result = {}
    local regex = string.format("([^%s]+)", sep)
    for part in string.gmatch(str, regex) do
        table.insert(result, part)
    end
    return result
end

-- Utility function for creating a deep copy of a simple table (array)
local function deep_copy(orig)
    local copy = {}
    for i, v in ipairs(orig) do
        copy[i] = v
    end
    return copy
end

-- Utility function to generate permutations of a table
local function generate_permutations(items)
    local results = {}
    local n = #items
    local p = {} -- Permutation array (indexes)
    for i = 1, n do p[i] = i end
    local i = 1

    while i <= n do
        p[i] = p[i] - 1
        local j = (i % 2 == 1) and 1 or p[i]
        -- Swap items[j] and items[i]
        items[j], items[i] = items[i], items[j]

        -- Add current permutation to results
        local current_permutation = {}
        for k = 1, n do current_permutation[k] = items[k] end
        table.insert(results, current_permutation)

        i = 1
        while i <= n and p[i] == 0 do
            p[i] = i
            i = i + 1
        end
    end

    -- Add the initial permutation as well (Heap's algorithm starts after the first)
     local initial_permutation = {}
     -- Need to reset items to original state if the algorithm modifies it in place
     -- A simpler recursive approach might be clearer, but Heap's is efficient.
     -- Let's use a simpler recursive one for clarity here.

     local function permute_recursive(arr, l, r, res)
         l = l or 1
         r = r or #arr
         res = res or {}
         if l == r then
             local copy = {}
             for idx = 1, #arr do copy[idx] = arr[idx] end
             table.insert(res, copy)
         else
             for i = l, r do
                 -- Swap
                 arr[l], arr[i] = arr[i], arr[l]
                 -- Recurse
                 permute_recursive(arr, l + 1, r, res)
                 -- Backtrack (swap back)
                 arr[l], arr[i] = arr[i], arr[l]
             end
         end
         return res
     end

     -- Reset items table to original state if needed, or just use a copy
     local items_copy = {}
     for k=1, #items do items_copy[k] = items[k] end
     return permute_recursive(items_copy)

end


-- Intcode Computer Simulation (Coroutine-based for Part 2)
local function create_intcode_computer(program_code)
    local memory = deep_copy(program_code)
    local ip = 1 -- Lua uses 1-based indexing
    local relative_base = 0 -- For Day 9 compatibility, though not needed for Day 7

    local co = coroutine.create(function(initial_input_provider)
        local input_queue = {}
        local first_input_values = initial_input_provider() -- Get initial values (e.g., phase)
        for _, val in ipairs(first_input_values) do
            table.insert(input_queue, val)
        end

        while true do
            local instruction = memory[ip]
            local opcode = instruction % 100

            local modes = {}
            modes[1] = math.floor(instruction / 100) % 10
            modes[2] = math.floor(instruction / 1000) % 10
            modes[3] = math.floor(instruction / 10000) % 10 -- Usually 0 for writes

            local function get_addr(offset)
                local mode = modes[offset]
                local addr
                if mode == 0 then -- Position Mode
                    addr = memory[ip + offset]
                elseif mode == 1 then -- Immediate Mode (Invalid for write addresses)
                     error("Immediate mode used for write address parameter " .. offset .. " at ip " .. ip)
                elseif mode == 2 then -- Relative Mode
                    addr = memory[ip + offset] + relative_base
                else
                    error("Unknown parameter mode: " .. mode .. " for parameter " .. offset .. " at ip " .. ip)
                end
                 -- Ensure address is valid (Lua tables auto-expand, but use 1-based index)
                 return addr + 1 -- Convert 0-based Intcode addresses to 1-based Lua indices
            end

            local function get_param(offset)
                local mode = modes[offset]
                local val
                if mode == 0 then -- Position Mode
                    local addr = memory[ip + offset] + 1
                    val = memory[addr] or 0 -- Default to 0 if address is out of initial bounds
                elseif mode == 1 then -- Immediate Mode
                    val = memory[ip + offset]
                elseif mode == 2 then -- Relative Mode
                    local addr = memory[ip + offset] + relative_base + 1
                    val = memory[addr] or 0
                else
                     error("Unknown parameter mode: " .. mode .. " for parameter " .. offset .. " at ip " .. ip)
                end
                return val
            end

            if opcode == 1 then -- Add
                local val1 = get_param(1)
                local val2 = get_param(2)
                local addr = get_addr(3)
                memory[addr] = val1 + val2
                ip = ip + 4
            elseif opcode == 2 then -- Multiply
                local val1 = get_param(1)
                local val2 = get_param(2)
                local addr = get_addr(3)
                memory[addr] = val1 * val2
                ip = ip + 4
            elseif opcode == 3 then -- Input
                if #input_queue == 0 then
                    -- Yield to request more input
                    local next_input = coroutine.yield("INPUT")
                    if next_input == nil then
                        error("Received nil input when expecting a value")
                    end
                    table.insert(input_queue, next_input)
                end
                local addr = get_addr(1)
                memory[addr] = table.remove(input_queue, 1)
                ip = ip + 2
            elseif opcode == 4 then -- Output
                local output_value = get_param(1)
                ip = ip + 2
                -- Yield the output value, expect next input (if any) upon resume
                local next_input = coroutine.yield(output_value)
                 if next_input ~= nil and next_input ~= "INPUT" then -- Check if resumed with data
                    table.insert(input_queue, next_input)
                 end
            elseif opcode == 5 then -- Jump-if-true
                local val1 = get_param(1)
                local val2 = get_param(2)
                if val1 ~= 0 then
                    ip = val2 + 1 -- Convert 0-based target to 1-based Lua index
                else
                    ip = ip + 3
                end
            elseif opcode == 6 then -- Jump-if-false
                local val1 = get_param(1)
                local val2 = get_param(2)
                if val1 == 0 then
                    ip = val2 + 1 -- Convert 0-based target to 1-based Lua index
                else
                    ip = ip + 3
                end
            elseif opcode == 7 then -- Less than
                local val1 = get_param(1)
                local val2 = get_param(2)
                local addr = get_addr(3)
                memory[addr] = (val1 < val2) and 1 or 0
                ip = ip + 4
            elseif opcode == 8 then -- Equals
                local val1 = get_param(1)
                local val2 = get_param(2)
                local addr = get_addr(3)
                memory[addr] = (val1 == val2) and 1 or 0
                ip = ip + 4
            elseif opcode == 9 then -- Adjust relative base (Not used in Day 7)
                 local val1 = get_param(1)
                 relative_base = relative_base + val1
                 ip = ip + 2
            elseif opcode == 99 then -- Halt
                -- Coroutine finishes naturally
                -- Yield HALT to signal completion explicitly if needed, or just let it end.
                coroutine.yield("HALT")
                break -- Exit the loop
            else
                error("Unknown opcode: " .. opcode .. " at position " .. ip)
            end
        end
    end)

    return co
end

-- Function to run the amplifier chain for Part 1
local function run_amplifier_chain_p1(program_code, phase_settings)
    local current_signal = 0

    for i = 1, 5 do
        local phase = phase_settings[i]
        local computer = create_intcode_computer(program_code)

        -- Provide phase setting
        local status, result = coroutine.resume(computer, function() return {phase} end)
        if status and result == "INPUT" then
             -- Computer is waiting for the signal input
             status, result = coroutine.resume(computer, current_signal)
             if status and result ~= "HALT" and result ~= "INPUT" then
                 current_signal = result -- Capture the output
                 -- Check if it halted immediately after output
                 local halt_status, halt_result = coroutine.resume(computer)
                 if not (halt_status and halt_result == "HALT") and coroutine.status(computer) ~= "dead" then
                      -- It produced output but didn't halt, which is unexpected for Part 1 amps
                      -- Or maybe it needs more input/output cycles? Part 1 description implies one output.
                      -- Let's assume it halts after one output for Part 1.
                 end
             elseif coroutine.status(computer) == "dead" then
                 -- It might have halted without outputting if the program allows,
                 -- or output might be the last value returned by resume. Need to check specification.
                 -- Let's assume result contains the last output if it didn't yield HALT.
                 if type(result) == "number" then current_signal = result end

             elseif status and result == "HALT" then
                 -- Halted without producing output after signal input? Unlikely based on problem.
                 -- Keep current_signal as is (might be 0 if first amp halts early)
             else
                 error("Amplifier " .. i .. " failed or yielded unexpectedly. Status: " .. tostring(status) .. ", Result: " .. tostring(result))
             end

        else
            error("Amplifier " .. i .. " failed during phase input or didn't request signal input.")
        end
    end
    return current_signal
end


-- Function to run the amplifier feedback loop for Part 2
local function run_feedback_loop(program_code, phase_settings)
    local amplifiers = {}
    local num_amps = 5

    -- Initialize amplifiers with phase settings
    for i = 1, num_amps do
        local phase = phase_settings[i]
        amplifiers[i] = create_intcode_computer(program_code)
        -- Initial resume provides the phase setting via the closure
        local status, result = coroutine.resume(amplifiers[i], function() return {phase} end)
        if not status or (result ~= "INPUT" and result ~= "HALT" and coroutine.status(amplifiers[i]) ~= "dead") then
             -- Should yield "INPUT" waiting for the first signal, or halt immediately (unlikely)
             -- Or it could potentially yield an output right away if the program is structured that way?
             -- Let's assume it waits for input.
             if result == "HALT" or coroutine.status(amplifiers[i]) == "dead" then
                  -- Amplifier halted immediately after phase setting.
             elseif result ~= "INPUT" then
                  print("Warning: Amp "..i.." did not yield INPUT after phase. Result: "..tostring(result))
                  -- It might have produced an output? Needs careful handling if so.
                  -- For now, assume it should wait for input.
             end
        elseif not status then
             error("Amplifier " .. i .. " crashed during initialization with phase " .. phase .. ". Error: " .. tostring(result))
        end
    end

    local current_signal = 0
    local last_output_e = 0
    local active_amps = num_amps
    local amp_index = 1

    while active_amps > 0 do
        local current_amp = amplifiers[amp_index]

        if coroutine.status(current_amp) == "suspended" then
            local status, result = coroutine.resume(current_amp, current_signal)

            if not status then
                error("Amplifier " .. amp_index .. " crashed. Error: " .. tostring(result))
            end

            if result == "INPUT" then
                -- Amplifier is waiting for input again, but we just gave it some.
                -- This implies it might need multiple inputs per cycle, or the loop structure is wrong.
                -- Or maybe it yielded INPUT last time and we are resuming it now.
                -- Let's assume it processed the input and is waiting for the *next* one.
                -- We'll provide input again on the next round for this amp.
            elseif result == "HALT" or coroutine.status(current_amp) == "dead" then
                -- Amplifier halted.
                active_amps = active_amps - 1
                -- If it's E halting, its *last* successfully yielded output is important, handled below.
            else -- It produced an output
                current_signal = result
                if amp_index == num_amps then
                    last_output_e = current_signal -- Track the last output from Amp E
                end
            end
        elseif coroutine.status(current_amp) == "dead" then
             -- This amp already halted, do nothing. Check handled by active_amps > 0
        else
             -- Status is 'running' (shouldn't happen here) or 'normal' (finished without HALT? Unlikely for Intcode)
             error("Unexpected coroutine status for amp " .. amp_index .. ": " .. coroutine.status(current_amp))
        end

        -- Move to the next amplifier, wrapping around
        amp_index = (amp_index % num_amps) + 1

        -- Optimization: If all amps are dead, exit early
        -- The check `active_amps > 0` handles this, but this prevents an unnecessary loop iteration.
        if active_amps == 0 then break end

    end

    return last_output_e
end

-- Main execution function
local function main()
    -- Read program code from input.txt
    local file = io.open("input.txt", "r")
    if not file then
        print("Error: Could not open input.txt")
        return
    end
    local content = file:read("*a")
    file:close()
    content = content:match("^%s*(.-)%s*$") -- Trim whitespace

    local program_str = split(content, ",")
    local program_code = {}
    for i, s in ipairs(program_str) do
        program_code[i] = tonumber(s)
        if not program_code[i] then
             print("Error: Invalid number '"..s.."' in input.")
             return
        end
    end

    -- --- Part 1 ---
    local max_signal_p1 = 0
    local phases_p1 = {0, 1, 2, 3, 4}
    local permutations_p1 = generate_permutations(phases_p1)

    for _, p in ipairs(permutations_p1) do
        local signal = run_amplifier_chain_p1(program_code, p)
        if signal > max_signal_p1 then
            max_signal_p1 = signal
        end
    end
    print("Part 1 Max Thruster Signal: " .. max_signal_p1)

    -- --- Part 2 ---
    local max_signal_p2 = 0
    local phases_p2 = {5, 6, 7, 8, 9}
    local permutations_p2 = generate_permutations(phases_p2)

    for _, p in ipairs(permutations_p2) do
        local signal = run_feedback_loop(program_code, p)
        if signal > max_signal_p2 then
            max_signal_p2 = signal
        end
    end
    print("Part 2 Max Thruster Signal: " .. max_signal_p2)

end

-- Run the main function
main()

