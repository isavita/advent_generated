
local function parse(input)
    local parts = {}
    local start = 1
    local function find_split(s, sep, start)
        local i, j = string.find(s, sep, start, true)
        if i then
            return i, j
        else
            return nil
        end
    end
    local i, j = find_split(input, "\n\n", start)
    if not i then return nil end
    table.insert(parts, string.sub(input, start, i - 1))
    table.insert(parts, string.sub(input, j + 1))
    if #parts ~= 2 then return nil end
    local gates = {}
    start = 1
    while true do
        local i, j = find_split(parts[2], "\n", start)
        local line = (i and string.sub(parts[2], start, i - 1)) or (start <= #parts[2] and string.sub(parts[2], start) or "")
        if line == "" then
          if not i then break end
          start = j + 1
          goto continue
        end
        local p = {}
        local s = 1
        local ii, jj = find_split(line, " -> ", s)
        if not ii then
          if not i then break end
          start = j + 1
          goto continue
        end
        table.insert(p, string.sub(line, s, ii - 1))
        table.insert(p, string.sub(line, jj + 1))

        if #p ~= 2 then
          if not i then break end
          start = j + 1
          goto continue
        end

        local gp = {}
        s = 1
        for k=1,3 do
            local iii, jjj = string.find(p[1], "%s+", s)
            if iii then
                table.insert(gp, string.sub(p[1], s, iii - 1))
                s = jjj + 1
            elseif k < 3 then
                if not i then break end
                start = j + 1
                goto continue
            else
              table.insert(gp, string.sub(p[1], s))
            end
        end

        if #gp ~= 3 then
          if not i then break end
          start = j + 1
          goto continue
        end

        table.insert(gates, {gate = {a = gp[1], op = gp[2], b = gp[3]}, output = p[2]})
        if not i then break end
        start = j + 1
        ::continue::
    end
    return gates
end

local function createLookups(gates)
    local lookup = {}
    local reverseLookup = {}
    for _, g in ipairs(gates) do
        lookup[g.output] = g.gate
        local inputs = {g.gate.a, g.gate.b}
        table.sort(inputs)
        local key = inputs[1] .. "_" .. g.gate.op .. "_" .. inputs[2]
        reverseLookup[key] = g.output
    end
    return lookup, reverseLookup
end

local function swap(pairs, gates, a, b)
    table.insert(pairs, {a, b})
    for i, g in ipairs(gates) do
        if g.output == a then
            gates[i].output = b
        elseif g.output == b then
            gates[i].output = a
        end
    end
end

local function getReverseLookupKey(a, op, b)
    local inputs = {a, b}
    table.sort(inputs)
    return inputs[1] .. "_" .. op .. "_" .. inputs[2]
end

local function solution(gates)
    local pairs = {}
    local numZ = 0
    for _, g in ipairs(gates) do
        if string.sub(g.output, 1, 1) == "z" then
            numZ = numZ + 1
        end
    end
    while #pairs < 4 do
        local adder = ""
        local carry = ""
        local lookup, reverseLookup = createLookups(gates)
        for i = 0, numZ - 1 do
            local xi = string.format("x%02d", i)
            local yi = string.format("y%02d", i)
            local zi = string.format("z%02d", i)
            if i == 0 then
                adder = reverseLookup[getReverseLookupKey(xi, "XOR", yi)]
                carry = reverseLookup[getReverseLookupKey(xi, "AND", yi)]
            else
                local bit = reverseLookup[getReverseLookupKey(xi, "XOR", yi)]
                if bit then
                    adder = reverseLookup[getReverseLookupKey(bit, "XOR", carry)]
                    if adder then
                        local c1 = reverseLookup[getReverseLookupKey(xi, "AND", yi)]
                        local c2 = reverseLookup[getReverseLookupKey(bit, "AND", carry)]
                        carry = reverseLookup[getReverseLookupKey(c1, "OR", c2)]
                    end
                end
            end
            if not adder or adder == "" then
                local gate = lookup[zi]
                local bitKey = getReverseLookupKey(xi, "XOR", yi)
                local bit = reverseLookup[bitKey]

                if reverseLookup[getReverseLookupKey(gate.a, "XOR", carry)] then
                    swap(pairs, gates, bit, gate.a)
                    break
                elseif reverseLookup[getReverseLookupKey(gate.b, "XOR", carry)] then
                    swap(pairs, gates, bit, gate.b)
                    break
                end
            elseif adder ~= zi then
                swap(pairs, gates, adder, zi)
                break
            end
        end
    end
    local result = {}
    for _, pair in ipairs(pairs) do
        table.insert(result, pair[1])
        table.insert(result, pair[2])
    end
    table.sort(result)
    return table.concat(result, ",")
end

local file = io.open("input.txt", "r")
if not file then
    print("Error reading input file")
    return
end
local input = file:read("*all")
file:close()
local gates = parse(input)
if not gates then
    print("Error parsing input")
    return
end
print(solution(gates))
