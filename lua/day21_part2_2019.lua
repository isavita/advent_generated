
local function readFile(filename)
    local f = io.open(filename, "r")
    if not f then return nil end
    local content = f:read("*a")
    f:close()
    return content
end

local function split(s, delimiter)
    local result = {}
    for match in (s .. delimiter):gmatch("(.-)" .. delimiter) do
        table.insert(result, match)
    end
    return result
end

local function newVM(filename)
    local code = {}
    local content = readFile(filename)
    local listStr = split(content:gsub("%s+", ""), ",")
    for i, v in ipairs(listStr) do
        code[i - 1] = tonumber(v)
    end
    return {
        code = code,
        ip = 0,
        relativeBase = 0,
        input = {},
        output = {}
    }
end

local function getParamAddress(vm, pos, mode)
    if mode == 0 then
        return vm.code[pos] or 0
    elseif mode == 1 then
        return pos
    elseif mode == 2 then
        return vm.relativeBase + (vm.code[pos] or 0)
    end
    error("wrong mode")
    return -1
end

local function getParamsAddresses(vm, pos, cmd, arity)
    local modes = {}
    local modeSection = math.floor(cmd / 100)
    for i = 0, arity - 1 do
        modes[i] = math.floor(modeSection / math.pow(10, i)) % 10
    end
    local results = {}
    for i = 0, arity - 1 do
        results[i] = getParamAddress(vm, pos + i + 1, modes[i])
    end
    return results
end

local function run(vm)
    local arity
    while true do
        local cmd = vm.code[vm.ip] or 0
        local opcode = cmd % 100

        if opcode == 1 then
            arity = 3
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            vm.code[params[2]] = (vm.code[params[0]] or 0) + (vm.code[params[1]] or 0)
        elseif opcode == 2 then
            arity = 3
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            vm.code[params[2]] = (vm.code[params[0]] or 0) * (vm.code[params[1]] or 0)
        elseif opcode == 3 then
            arity = 1
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            vm.code[params[0]] = table.remove(vm.input, 1)
        elseif opcode == 4 then
            arity = 1
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            table.insert(vm.output, vm.code[params[0]] or 0)
        elseif opcode == 5 then
            arity = 2
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            if (vm.code[params[0]] or 0) ~= 0 then
                vm.ip = vm.code[params[1]] or 0
                goto continue
            end
        elseif opcode == 6 then
            arity = 2
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            if (vm.code[params[0]] or 0) == 0 then
                vm.ip = vm.code[params[1]] or 0
                goto continue
            end
        elseif opcode == 7 then
            arity = 3
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            vm.code[params[2]] = ((vm.code[params[0]] or 0) < (vm.code[params[1]] or 0)) and 1 or 0
        elseif opcode == 8 then
            arity = 3
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            vm.code[params[2]] = ((vm.code[params[0]] or 0) == (vm.code[params[1]] or 0)) and 1 or 0
        elseif opcode == 9 then
            arity = 1
            local params = getParamsAddresses(vm, vm.ip, cmd, arity)
            vm.relativeBase = vm.relativeBase + (vm.code[params[0]] or 0)
        elseif opcode == 99 then
            return
        else
            error("not an opcode " .. cmd)
        end

        vm.ip = vm.ip + arity + 1
        ::continue::
    end
end

local function sendString(input, s)
    for i = 1, #s do
        table.insert(input, string.byte(s, i, i))
    end
    table.insert(input, 10)
end

local vm = newVM("input.txt")

local instructions = {
    "NOT A J",
    "NOT B T",
    "OR T J",
    "NOT C T",
    "OR T J",
    "AND D J",
    "NOT A T",
    "AND A T",
    "OR E T",
    "OR H T",
    "AND T J",
    "RUN"
}

for _, i in ipairs(instructions) do
    sendString(vm.input, i)
end

run(vm)

for _, v in ipairs(vm.output) do
    if v > 127 then
        print(v)
    end
end
