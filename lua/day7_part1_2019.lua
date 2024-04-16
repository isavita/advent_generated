function read_input(filename)
    local file = io.open(filename, "r")
    local content = file:read("*all")
    file:close()
    local code = {}
    for num in string.gmatch(content, "[^,]+") do
        table.insert(code, tonumber(num))
    end
    return code
end

function get_param(code, address, immediate)
    local param = code[address + 1]
    if immediate then
        return param
    else
        return code[param + 1]
    end
end

function run(code, inputs)
    local input_index = 1
    local output = nil
    local ip = 1
    while true do
        local cmd = code[ip]
        local opcode = cmd % 100
        local immediate1 = math.floor(cmd / 100) % 10 == 1
        local immediate2 = math.floor(cmd / 1000) % 10 == 1
        local immediate3 = math.floor(cmd / 10000) % 10 == 1

        if opcode == 1 then
            local param1 = get_param(code, ip, immediate1)
            local param2 = get_param(code, ip + 1, immediate2)
            local address = get_param(code, ip + 2, true)
            code[address + 1] = param1 + param2
            ip = ip + 4
        elseif opcode == 2 then
            local param1 = get_param(code, ip, immediate1)
            local param2 = get_param(code, ip + 1, immediate2)
            local address = get_param(code, ip + 2, true)
            code[address + 1] = param1 * param2
            ip = ip + 4
        elseif opcode == 3 then
            local address = get_param(code, ip, true)
            code[address + 1] = inputs[input_index]
            input_index = input_index + 1
            ip = ip + 2
        elseif opcode == 4 then
            local param1 = get_param(code, ip, immediate1)
            output = param1
            ip = ip + 2
        elseif opcode == 5 then
            local param1 = get_param(code, ip, immediate1)
            local param2 = get_param(code, ip + 1, immediate2)
            if param1 ~= 0 then
                ip = param2 + 1
            else
                ip = ip + 3
            end
        elseif opcode == 6 then
            local param1 = get_param(code, ip, immediate1)
            local param2 = get_param(code, ip + 1, immediate2)
            if param1 == 0 then
                ip = param2 + 1
            else
                ip = ip + 3
            end
        elseif opcode == 7 then
            local param1 = get_param(code, ip, immediate1)
            local param2 = get_param(code, ip + 1, immediate2)
            local address = get_param(code, ip + 2, true)
            if param1 < param2 then
                code[address + 1] = 1
            else
                code[address + 1] = 0
            end
            ip = ip + 4
        elseif opcode == 8 then
            local param1 = get_param(code, ip, immediate1)
            local param2 = get_param(code, ip + 1, immediate2)
            local address = get_param(code, ip + 2, true)
            if param1 == param2 then
                code[address + 1] = 1
            else
                code[address + 1] = 0
            end
            ip = ip + 4
        elseif opcode == 99 then
            break
        else
            error("Invalid opcode: " .. opcode)
        end
    end
    return output
end

function permutations(arr)
    local function permute(a, n)
        n = n or #a
        if n <= 1 then
            coroutine.yield(a)
        else
            for i = 1, n do
                a[n], a[i] = a[i], a[n]
                permute(a, n - 1)
                a[n], a[i] = a[i], a[n]
            end
        end
    end
    return coroutine.wrap(function() permute(arr) end)
end

function run_amplifiers(phase)
    local last_output = 0
    for i = 1, 5 do
        local code = read_input("input.txt")
        last_output = run(code, {phase[i], last_output})
    end
    return last_output
end

function main()
    local max_output = 0
    for phase in permutations({0, 1, 2, 3, 4}) do
        local output = run_amplifiers(phase)
        if output > max_output then
            max_output = output
        end
    end
    print(max_output)
end

main()