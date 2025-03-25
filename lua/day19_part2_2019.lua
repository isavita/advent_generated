
local function split(s, delimiter)
    local result = {}
    for match in (s..delimiter):gmatch("(.-)"..delimiter) do
        table.insert(result, match)
    end
    return result
end

local function load(filename)
    local file = io.open(filename, "r")
    local content = file:read("*a")
    file:close()
    local data = split(content, ",")
    local code = {}
    for i, v in ipairs(data) do
        code[i - 1] = tonumber(v)
    end
    return code
end

local function get_param(code, pos, mode, relative_base)
    if mode == 0 then
        return code[code[pos] or 0] or 0
    elseif mode == 1 then
        return code[pos] or 0
    else
        return code[(relative_base + (code[pos] or 0))] or 0
    end
end

local function set_param(code, pos, mode, relative_base, value)
    if mode == 0 then
        code[code[pos] or 0] = value
    elseif mode == 2 then
        code[relative_base + (code[pos] or 0)] = value
    end
end
local function run(code, input)
    local ip = 0
    local relative_base = 0
    local output = {}
    local input_index = 1

    while true do
        local cmd = code[ip] or 0
        local opcode = cmd % 100
        local mode1 = math.floor(cmd / 100) % 10
        local mode2 = math.floor(cmd / 1000) % 10
        local mode3 = math.floor(cmd / 10000) % 10

        if opcode == 1 then
            local val1 = get_param(code, ip + 1, mode1, relative_base)
            local val2 = get_param(code, ip + 2, mode2, relative_base)
            set_param(code, ip+3, mode3, relative_base, val1 + val2)
            ip = ip + 4
        elseif opcode == 2 then
            local val1 = get_param(code, ip + 1, mode1, relative_base)
            local val2 = get_param(code, ip + 2, mode2, relative_base)
            set_param(code, ip+3, mode3, relative_base, val1 * val2)

            ip = ip + 4
        elseif opcode == 3 then
            set_param(code, ip + 1, mode1, relative_base, input[input_index])
            input_index = input_index + 1
            ip = ip + 2
        elseif opcode == 4 then
            table.insert(output, get_param(code, ip + 1, mode1, relative_base))
            ip = ip + 2
        elseif opcode == 5 then
            local val1 = get_param(code, ip + 1, mode1, relative_base)
            local val2 = get_param(code, ip + 2, mode2, relative_base)
            if val1 ~= 0 then
                ip = val2
            else
                ip = ip + 3
            end
        elseif opcode == 6 then
            local val1 = get_param(code, ip + 1, mode1, relative_base)
            local val2 = get_param(code, ip + 2, mode2, relative_base)
            if val1 == 0 then
                ip = val2
            else
                ip = ip + 3
            end
        elseif opcode == 7 then
            local val1 = get_param(code, ip + 1, mode1, relative_base)
            local val2 = get_param(code, ip + 2, mode2, relative_base)
            set_param(code, ip+3, mode3, relative_base, (val1 < val2 and 1 or 0))
            ip = ip + 4
        elseif opcode == 8 then
            local val1 = get_param(code, ip + 1, mode1, relative_base)
            local val2 = get_param(code, ip + 2, mode2, relative_base)
            set_param(code, ip+3, mode3, relative_base, (val1 == val2 and 1 or 0))

            ip = ip + 4
        elseif opcode == 9 then
            local val1 = get_param(code, ip + 1, mode1, relative_base)
            relative_base = relative_base + val1
            ip = ip + 2
        elseif opcode == 99 then
            break
        else
           error("bad opcode")
        end
    end
     return output[#output]
end

local function beam(x, y)
    local code = load("input.txt")
     return run(code, {x, y}) == 1
end

local function main()
    local y = 20
    local x = 0

    while true do
        if not beam(x, y) then
            x = x + 1
        elseif not beam(x + 99, y) then
            y = y + 1
        elseif not beam(x, y + 99) then
             x = x+1
        else
            print(x * 10000 + y)
            return
        end
    end
end

main()
