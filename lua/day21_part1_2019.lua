
local function split(s, delimiter)
    local result = {}
    for match in (s..delimiter):gmatch("(.-)"..delimiter) do
        table.insert(result, match)
    end
    return result
end

local function load_program(filename)
    local file = io.open(filename, "r")
    local content = file:read("*a")
    file:close()
    local list_str = split(content:gsub("%s+", ""), ",")
    local code = {}
    for i, val in ipairs(list_str) do
        code[i - 1] = tonumber(val)
    end
    return code
end

local function get_param(code, ip, relative_base, mode, index)
    local addr = code[ip + index] or 0
    if mode == 0 then
        return code[addr] or 0
    elseif mode == 1 then
        return addr
    else
        return code[relative_base + addr] or 0
    end
end

local function get_address(code, ip, relative_base, mode, index)
    local addr = code[ip + index] or 0
    if mode == 0 then
        return addr
    else
        return relative_base + addr
    end
end

local function run_vm(code, input)
    local ip = 0
    local relative_base = 0
    local output = {}

    while true do
        local cmd = code[ip] or 0
        local opcode = cmd % 100
        local modes = {
            math.floor(cmd / 100) % 10,
            math.floor(cmd / 1000) % 10,
            math.floor(cmd / 10000) % 10
        }

        if opcode == 1 then
            code[get_address(code, ip, relative_base, modes[3], 3)] = get_param(code, ip, relative_base, modes[1], 1) + get_param(code, ip, relative_base, modes[2], 2)
            ip = ip + 4
        elseif opcode == 2 then
            code[get_address(code, ip, relative_base, modes[3], 3)] = get_param(code, ip, relative_base, modes[1], 1) * get_param(code, ip, relative_base, modes[2], 2)
            ip = ip + 4
        elseif opcode == 3 then
            code[get_address(code, ip, relative_base, modes[1], 1)] = table.remove(input, 1)
            ip = ip + 2
        elseif opcode == 4 then
            table.insert(output, get_param(code, ip, relative_base, modes[1], 1))
            ip = ip + 2
        elseif opcode == 5 then
            if get_param(code, ip, relative_base, modes[1], 1) ~= 0 then
                ip = get_param(code, ip, relative_base, modes[2], 2)
            else
                ip = ip + 3
            end
        elseif opcode == 6 then
            if get_param(code, ip, relative_base, modes[1], 1) == 0 then
                ip = get_param(code, ip, relative_base, modes[2], 2)
            else
                ip = ip + 3
            end
        elseif opcode == 7 then
            code[get_address(code, ip, relative_base, modes[3], 3)] = get_param(code, ip, relative_base, modes[1], 1) < get_param(code, ip, relative_base, modes[2], 2) and 1 or 0
            ip = ip + 4
        elseif opcode == 8 then
            code[get_address(code, ip, relative_base, modes[3], 3)] = get_param(code, ip, relative_base, modes[1], 1) == get_param(code, ip, relative_base, modes[2], 2) and 1 or 0
            ip = ip + 4
        elseif opcode == 9 then
            relative_base = relative_base + get_param(code, ip, relative_base, modes[1], 1)
            ip = ip + 2
        elseif opcode == 99 then
            break
        end
    end
    return output
end
local function send_string(input_table, s)
    for r in s:gmatch(".") do
        table.insert(input_table, string.byte(r))
    end
   table.insert(input_table, 10)
end

local function main()
    local code = load_program("input.txt")
    local input = {}
    local instructions = {
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "WALK",
    }
        for _, instr in ipairs(instructions) do
        send_string(input, instr)
    end
    local output = run_vm(code, input)
     for _,v in ipairs(output) do
        if v > 127 then
            print(v)
            break
        end
    end

end

main()
