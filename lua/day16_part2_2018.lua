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
    local test_program = {}
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input.txt") end
    
    local before, instruction, after
    local parsing_samples = true
    local line_count = 0
    local blank_line_count = 0

    for line in file:lines() do
        line_count = line_count + 1
        if #line == 0 then
            blank_line_count = blank_line_count + 1
            if blank_line_count >= 2 then
                parsing_samples = false
            end
        else
            blank_line_count = 0
            if parsing_samples then
                if line:match("Before:") then
                    before = {line:match("(%d+), (%d+), (%d+), (%d+)")}
                    for i = 1, 4 do before[i] = tonumber(before[i]) end
                elseif line:match("After:") then
                    after = {line:match("(%d+), (%d+), (%d+), (%d+)")}
                    for i = 1, 4 do after[i] = tonumber(after[i]) end
                    if before and instruction and after then
                        table.insert(samples, {before = before, instruction = instruction, after = after})
                        before, instruction, after = nil, nil, nil
                    else
                        print("Warning: Incomplete sample at line " .. line_count)
                    end
                else
                    instruction = {line:match("(%d+) (%d+) (%d+) (%d+)")}
                    if instruction then
                        for i = 1, 4 do instruction[i] = tonumber(instruction[i]) end
                    else
                        print("Warning: Invalid instruction at line " .. line_count)
                    end
                end
            else
                local instr = {line:match("(%d+) (%d+) (%d+) (%d+)")}
                if instr then
                    for i = 1, 4 do instr[i] = tonumber(instr[i]) end
                    table.insert(test_program, instr)
                else
                    print("Warning: Invalid test program instruction at line " .. line_count)
                end
            end
        end
    end
    file:close()
    print("Parsed " .. #samples .. " samples and " .. #test_program .. " test program instructions")
    return samples, test_program
end

-- Function to test a sample against all opcodes
local function test_sample(sample)
    local matching_opcodes = {}
    for name, opcode in pairs(opcodes) do
        local registers = {sample.before[1], sample.before[2], sample.before[3], sample.before[4]}
        opcode(registers, sample.instruction[2], sample.instruction[3], sample.instruction[4])
        if registers[1] == sample.after[1] and
           registers[2] == sample.after[2] and
           registers[3] == sample.after[3] and
           registers[4] == sample.after[4] then
            table.insert(matching_opcodes, name)
        end
    end
    return matching_opcodes
end

-- Helper function to check if a table contains a value
local function table_contains(table, element)
    for _, value in pairs(table) do
        if value == element then
            return true
        end
    end
    return false
end

-- Function to determine opcode numbers
local function determine_opcodes(samples)
    if not samples or #samples == 0 then
        error("No samples provided to determine_opcodes")
    end

    local possible_opcodes = {}
    for i = 0, 15 do
        possible_opcodes[i] = {}
        for name, _ in pairs(opcodes) do
            possible_opcodes[i][name] = true
        end
    end

    for _, sample in ipairs(samples) do
        local matching = test_sample(sample)
        for name, _ in pairs(opcodes) do
            if not table_contains(matching, name) then
                possible_opcodes[sample.instruction[1]][name] = nil
            end
        end
    end

    local opcode_map = {}
    local iterations = 0
    while true do
        local assigned = false
        for num, possible in pairs(possible_opcodes) do
            local single_opcode = next(possible)
            if single_opcode and next(possible, single_opcode) == nil then
                opcode_map[num] = single_opcode
                for _, other_possible in pairs(possible_opcodes) do
                    other_possible[single_opcode] = nil
                end
                assigned = true
            end
        end
        if not assigned then break end
        iterations = iterations + 1
        if iterations > 16 then
            error("Unable to determine unique opcode mappings")
        end
    end

    print("Determined " .. #opcode_map .. " opcode mappings")
    return opcode_map
end

-- Function to execute the test program
local function execute_program(program, opcode_map)
    local registers = {0, 0, 0, 0}
    for _, instruction in ipairs(program) do
        local opcode = opcode_map[instruction[1]]
        if not opcode then
            error("Unknown opcode number: " .. instruction[1])
        end
        opcodes[opcode](registers, instruction[2], instruction[3], instruction[4])
    end
    return registers[1]
end

-- Main function
local function main()
    local samples, test_program = parse_input()
    if #samples == 0 then
        error("No samples parsed from input")
    end
    local opcode_map = determine_opcodes(samples)
    if not opcode_map or next(opcode_map) == nil then
        error("Failed to determine opcode mappings")
    end
    local result = execute_program(test_program, opcode_map)
    print("Value in register 0 after executing the test program:", result)
end

main()
