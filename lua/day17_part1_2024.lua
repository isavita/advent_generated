
-- Requires Lua 5.3+ for bitwise operators

local function main()
    local A, B, C = 0, 0, 0
    local program = {}

    local file = io.open("input.txt", "r")
    if not file then
        return
    end

    for line in file:lines() do
        line = line:match("^%s*(.-)%s*$")
        if line and #line > 0 then
            local val_str = line:match("^Register A:%s*(-?%d+)")
            if val_str then A = tonumber(val_str) end
            val_str = line:match("^Register B:%s*(-?%d+)")
            if val_str then B = tonumber(val_str) end
            val_str = line:match("^Register C:%s*(-?%d+)")
            if val_str then C = tonumber(val_str) end

            local prog_list_str = line:match("^Program:%s*(.+)")
            if prog_list_str then
                for num_str in prog_list_str:gmatch("[^,]+") do
                    table.insert(program, tonumber(num_str:match("^%s*(-?%d+)%s*$")))
                end
            end
        end
    end
    file:close()

    local function get_combo_val(op)
        if op >= 0 and op <= 3 then return op end
        if op == 4 then return A end
        if op == 5 then return B end
        if op == 6 then return C end
        error("invalid combo operand: " .. op)
    end

    local output_vals = {}
    local ip = 1
    local prog_len = #program
    local floor = math.floor

    while ip <= prog_len do
        local opcode = program[ip]
        if ip + 1 > prog_len then break end
        local operand = program[ip + 1]
        local next_ip = ip + 2

        if opcode == 0 then
            local den = get_combo_val(operand)
            A = (den ~= 0) and floor(A / (1 << den)) or 0
        elseif opcode == 1 then
            B = B ~ operand
        elseif opcode == 2 then
            B = get_combo_val(operand) % 8
        elseif opcode == 3 then
            if A ~= 0 then
                next_ip = operand + 1 -- Adjust 0-based target index to 1-based Lua index
            end
        elseif opcode == 4 then
            B = B ~ C
        elseif opcode == 5 then
            table.insert(output_vals, tostring(get_combo_val(operand) % 8))
        elseif opcode == 6 then
            local val = get_combo_val(operand)
            B = floor(A / (1 << val))
        elseif opcode == 7 then
            local val = get_combo_val(operand)
            C = floor(A / (1 << val))
        else
             break
        end
        ip = next_ip
        if ip <= 0 or ip > prog_len +1 then -- Basic jump sanity check
             break
        end
    end

    print(table.concat(output_vals, ","))
end

main()
