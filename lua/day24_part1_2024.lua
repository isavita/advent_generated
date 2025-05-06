
-- Helper function for binary to decimal conversion, compatible with Lua 5.1+
-- Expects a string of '0's and '1's, MSB first.
local function binary_to_decimal(bin_str)
    local dec = 0
    local power = 0
    for i = #bin_str, 1, -1 do -- Iterate from LSB of string (rightmost char) to MSB (leftmost char)
        local char = bin_str:sub(i, i)
        if char == '1' then
            dec = dec + (2^power)
        elseif char ~= '0' then
            -- This indicates an invalid character in the binary string.
            io.stderr:write("Warning: Invalid character '" .. char .. "' in binary string: " .. bin_str .. "\n")
            return nil -- Indicate error
        end
        power = power + 1
    end
    return dec
end

-- Main function to encapsulate the program logic
local function main()
    local wire_values = {} -- Stores current values of wires {["wire_name"] = 0_or_1}
    local gates = {}       -- Stores gate definitions: list of {in1, op, in2, out_wire}

    -- Define gate operations
    -- These functions expect numeric inputs (0 or 1) and return 0 or 1.
    local operations = {
        AND = function(a, b) return (a == 1 and b == 1) and 1 or 0 end,
        OR  = function(a, b) return (a == 1 or b == 1) and 1 or 0 end,
        XOR = function(a, b) return (a ~= b) and 1 or 0 end, -- (a == 1 and b == 0) or (a == 0 and b == 1)
    }

    -- Phase 1: Parse input from input.txt
    local file = io.open("input.txt", "r")
    if not file then
        io.stderr:write("Error: Could not open input.txt\n")
        return -- Exit main function on file error
    end

    for line in file:lines() do
        line = line:match("^%s*(.-)%s*$") -- Trim leading/trailing whitespace
        if line ~= "" then -- Skip empty lines
            -- Try to match initial wire value: "wire_name: value"
            local w_name, w_val_str = line:match("^(%w+):%s*(%d+)$")
            if w_name then
                wire_values[w_name] = tonumber(w_val_str)
            else
                -- Try to match gate definition: "input1_wire GATE_TYPE input2_wire -> output_wire"
                local in1, op, in2, out_w = line:match("^(%w+)%s+(%u+)%s+(%w+)%s+->%s+(%w+)$")
                if in1 then
                    table.insert(gates, {in1 = in1, op = op, in2 = in2, out_wire = out_w})
                else
                    -- Optional: Log lines that don't match expected formats if strictness is required
                    -- io.stderr:write("Warning: Unrecognized line format: " .. line .. "\n")
                end
            end
        end
    end
    file:close()

    -- Phase 2: Simulate the circuit
    local made_progress
    repeat
        made_progress = false
        for _, gate_info in ipairs(gates) do
            -- Only process this gate if its output wire doesn't have a value yet
            if wire_values[gate_info.out_wire] == nil then
                local val1 = wire_values[gate_info.in1]
                local val2 = wire_values[gate_info.in2]

                -- Both input wires must have values for the gate to operate
                if val1 ~= nil and val2 ~= nil then
                    local op_func = operations[gate_info.op]
                    if op_func then
                        wire_values[gate_info.out_wire] = op_func(val1, val2)
                        made_progress = true -- A gate fired, so progress was made in this pass
                    else
                        io.stderr:write("Error: Unknown gate operation '" .. gate_info.op .. 
                                        "' for gate outputting to " .. gate_info.out_wire .. "\n")
                    end
                end
            end
        end
    until not made_progress -- Repeat until a full pass over all gates yields no new wire values

    -- Phase 3: Calculate the final output value from 'z' wires
    local z_wires_found = {} -- Stores names of wires starting with 'z' and ending with digits
    for wire_name_key, _ in pairs(wire_values) do
        if wire_name_key:match("^z%d+$") then -- e.g., z0, z00, z12 (not 'zebra')
            table.insert(z_wires_found, wire_name_key)
        end
    end
    
    if #z_wires_found == 0 then
        print(0) -- Default output if no 'z' wires are found or resolved
        return
    end

    -- Sort 'z' wires numerically by their suffix (e.g., z0, z1, z2, ..., z10)
    table.sort(z_wires_found, function(a, b)
        -- Extract numeric part (e.g., "00" from "z00") and convert to number for comparison
        return tonumber(a:sub(2)) < tonumber(b:sub(2))
    end)

    -- Construct the binary string: MSB first.
    -- "z00 is the least significant bit, then z01, then z02, and so on."
    -- So, if sorted z_wires_found = {z00, z01, z02}, binary string is value(z02)value(z01)value(z00).
    local binary_string_parts = {}
    local all_z_values_resolved = true
    for i = #z_wires_found, 1, -1 do -- Iterate from highest index (e.g., zN) down to lowest (z00)
        local z_wire_name = z_wires_found[i]
        local z_wire_value = wire_values[z_wire_name]
        
        if z_wire_value == nil then
            -- This case should ideally not happen if problem implies all z-outputs are determined.
            io.stderr:write("Warning: Output wire " .. z_wire_name .. " has no value after simulation.\n")
            all_z_values_resolved = false
            break -- Stop processing if a required z-wire value is missing
        end
        table.insert(binary_string_parts, tostring(z_wire_value))
    end

    if not all_z_values_resolved then
        io.stderr:write("Error: Not all required 'z' wires resolved to a value. Cannot compute final number.\n")
        print(0) -- Fallback value
        return
    end

    local binary_string = table.concat(binary_string_parts)

    if binary_string == "" then
        -- This might occur if z_wires_found was initially populated but an issue arose,
        -- or if it was empty to begin with (though covered by the #z_wires_found == 0 check).
        print(0)
    else
        local decimal_value = binary_to_decimal(binary_string)
        if decimal_value == nil then
            -- binary_to_decimal returns nil on error (e.g., non-binary char in string)
            io.stderr:write("Error: Failed to convert binary string '"..binary_string.."' to decimal.\n")
            print(0) -- Fallback value
        else
            print(decimal_value)
        end
    end
end

-- Execute the main function of the program
main()
