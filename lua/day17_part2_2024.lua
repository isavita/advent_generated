
local computeOperand
local simulateComputer
local check

computeOperand = function(val, reg_a, reg_b, reg_c)
    if val >= 0 and val <= 3 then
        return val
    elseif val == 4 then
        return reg_a
    elseif val == 5 then
        return reg_b
    elseif val == 6 then
        return reg_c
    else
        error("Invalid combo operand: " .. tostring(val))
    end
end

simulateComputer = function(p_struct)
    local outs = {}
    local a = p_struct.a
    local b = p_struct.b
    local c = p_struct.c
    local program_code = p_struct.program_code

    local i = 1
    while i <= #program_code do
        local cmd = program_code[i]
        local operand_val_from_prog

        if cmd ~= 4 and i + 1 <= #program_code then
            operand_val_from_prog = program_code[i+1]
        elseif cmd ~= 4 and i + 1 > #program_code then
             error("Opcode " .. cmd .. " requires an operand, but program ended.")
        end

        if cmd == 0 then
            local op_arg = computeOperand(operand_val_from_prog, a, b, c)
            a = a >> op_arg
        elseif cmd == 1 then
            b = b ~ operand_val_from_prog
        elseif cmd == 2 then
            local op_arg = computeOperand(operand_val_from_prog, a, b, c)
            b = op_arg % 8
        elseif cmd == 3 then
            if a ~= 0 then
                local target_pc_0_indexed = operand_val_from_prog
                i = target_pc_0_indexed + 1
                goto continue_loop_simulate
            end
        elseif cmd == 4 then
            b = b ~ c
        elseif cmd == 5 then
            local op_arg = computeOperand(operand_val_from_prog, a, b, c)
            table.insert(outs, op_arg % 8)
        elseif cmd == 6 then
            local op_arg = computeOperand(operand_val_from_prog, a, b, c)
            b = a >> op_arg
        elseif cmd == 7 then
            local op_arg = computeOperand(operand_val_from_prog, a, b, c)
            c = a >> op_arg
        else
            error("Invalid opcode: " .. tostring(cmd))
        end

        i = i + 2
        ::continue_loop_simulate::
    end
    return outs
end

check = function(p_main)
    local valids = {}
    local stack = {{a = 0, b = 0}} 
    local seen = {} 

    local program_list_ref = p_main.program_code

    while #stack > 0 do
        local state = table.remove(stack) 
        local depth = state.a
        local score = state.b
        local key = depth .. "," .. tostring(score)

        if not seen[key] then
            seen[key] = true

            if depth == #program_list_ref then
                table.insert(valids, score)
            else
                for i_val = 0, 7 do
                    local newScore = i_val + 8 * score
                    local testProgram = {
                        a = newScore,
                        b = p_main.b,
                        c = p_main.c,
                        program_code = program_list_ref
                    }
                    local result = simulateComputer(testProgram)
                    
                    if #result > 0 and result[1] == program_list_ref[#program_list_ref - depth] then
                        table.insert(stack, {a = depth + 1, b = newScore})
                    end
                end
            end
        end
    end
    return valids
end

local function main()
    local file = io.open("input.txt", "r")
    if not file then
        io.stderr:write("Error: Cannot open input.txt\n")
        return
    end
    local content = file:read("*a")
    file:close()

    local initial_a, initial_b, initial_c
    local program_code_main

    for line in string.gmatch(content, "[^\r\n]+") do
        local val_str
        val_str = string.match(line, "^Register A:%s*(-?%d+)$")
        if val_str then initial_a = tonumber(val_str) goto next_line end

        val_str = string.match(line, "^Register B:%s*(-?%d+)$")
        if val_str then initial_b = tonumber(val_str) goto next_line end

        val_str = string.match(line, "^Register C:%s*(-?%d+)$")
        if val_str then initial_c = tonumber(val_str) goto next_line end

        local prog_str_match = string.match(line, "^Program:%s*(.+)$")
        if prog_str_match then
            local program_list = {}
            for num_str in string.gmatch(prog_str_match, "-?%d+") do
                table.insert(program_list, tonumber(num_str))
            end
            program_code_main = program_list
        end
        ::next_line::
    end
    
    if not initial_a or not initial_b or not initial_c or not program_code_main then
        io.stderr:write("Error: Input file format incorrect or incomplete.\n")
        return
    end

    local p_to_check = {
        a = initial_a, 
        b = initial_b,
        c = initial_c,
        program_code = program_code_main
    }

    local validValues = check(p_to_check)

    if #validValues == 0 then
        io.stderr:write("No valid values found.\n")
        return
    end

    local minVal = validValues[1]
    for i = 2, #validValues do
        if validValues[i] < minVal then
            minVal = validValues[i]
        end
    end

    print(minVal)
end

main()

