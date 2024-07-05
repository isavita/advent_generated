-- Implement bitwise operations if not available
local band, bor
if bit32 then
    band, bor = bit32.band, bit32.bor
else
    function band(a, b)
        local result = 0
        local bitval = 1
        while a > 0 and b > 0 do
            if a % 2 == 1 and b % 2 == 1 then
                result = result + bitval
            end
            bitval = bitval * 2
            a = math.floor(a / 2)
            b = math.floor(b / 2)
        end
        return result
    end

    function bor(a, b)
        local result = 0
        local bitval = 1
        while a > 0 or b > 0 do
            if a % 2 == 1 or b % 2 == 1 then
                result = result + bitval
            end
            bitval = bitval * 2
            a = math.floor(a / 2)
            b = math.floor(b / 2)
        end
        return result
    end
end

-- Define all 16 opcodes
local opcodes = {
    addr = function(r, a, b, c) r[c+1] = (r[a+1] or 0) + (r[b+1] or 0) end,
    addi = function(r, a, b, c) r[c+1] = (r[a+1] or 0) + b end,
    mulr = function(r, a, b, c) r[c+1] = (r[a+1] or 0) * (r[b+1] or 0) end,
    muli = function(r, a, b, c) r[c+1] = (r[a+1] or 0) * b end,
    banr = function(r, a, b, c) r[c+1] = band((r[a+1] or 0), (r[b+1] or 0)) end,
    bani = function(r, a, b, c) r[c+1] = band((r[a+1] or 0), b) end,
    borr = function(r, a, b, c) r[c+1] = bor((r[a+1] or 0), (r[b+1] or 0)) end,
    bori = function(r, a, b, c) r[c+1] = bor((r[a+1] or 0), b) end,
    setr = function(r, a, b, c) r[c+1] = r[a+1] or 0 end,
    seti = function(r, a, b, c) r[c+1] = a end,
    gtir = function(r, a, b, c) r[c+1] = a > (r[b+1] or 0) and 1 or 0 end,
    gtri = function(r, a, b, c) r[c+1] = (r[a+1] or 0) > b and 1 or 0 end,
    gtrr = function(r, a, b, c) r[c+1] = (r[a+1] or 0) > (r[b+1] or 0) and 1 or 0 end,
    eqir = function(r, a, b, c) r[c+1] = a == (r[b+1] or 0) and 1 or 0 end,
    eqri = function(r, a, b, c) r[c+1] = (r[a+1] or 0) == b and 1 or 0 end,
    eqrr = function(r, a, b, c) r[c+1] = (r[a+1] or 0) == (r[b+1] or 0) and 1 or 0 end
}

-- Function to parse input
local function parse_input()
    local samples = {}
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input.txt") end
    
    local before, instruction, after
    for line in file:lines() do
        if line:match("Before:") then
            before = {line:match("(%d+), (%d+), (%d+), (%d+)")}
            for i = 1, 4 do before[i] = tonumber(before[i]) end
        elseif line:match("After:") then
            after = {line:match("(%d+), (%d+), (%d+), (%d+)")}
            for i = 1, 4 do after[i] = tonumber(after[i]) end
            table.insert(samples, {before = before, instruction = instruction, after = after})
        elseif #line > 0 and not line:match("Before:") and not line:match("After:") then
            instruction = {line:match("(%d+) (%d+) (%d+) (%d+)")}
            for i = 1, 4 do instruction[i] = tonumber(instruction[i]) end
        end
    end
    file:close()
    return samples
end

-- Function to test a sample against all opcodes
local function test_sample(sample)
    local count = 0
    for name, opcode in pairs(opcodes) do
        local registers = {sample.before[1], sample.before[2], sample.before[3], sample.before[4]}
        local success, err = pcall(function()
            opcode(registers, sample.instruction[2], sample.instruction[3], sample.instruction[4])
        end)
        if not success then
            print("Error in opcode " .. name .. ": " .. err)
        elseif registers[1] == sample.after[1] and
               registers[2] == sample.after[2] and
               registers[3] == sample.after[3] and
               registers[4] == sample.after[4] then
            count = count + 1
        end
    end
    return count
end

-- Main function
local function main()
    local samples = parse_input()
    local count = 0
    for i, sample in ipairs(samples) do
        local matching = test_sample(sample)
        if matching >= 3 then
            count = count + 1
        end
    end
    print("Number of samples behaving like three or more opcodes:", count)
end

main()
