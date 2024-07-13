-- Helper function to parse a packet from a string
local function parse_packet(input)
    local function parse_list(s, i)
        local list = {}
        i = i + 1
        while i <= #s do
            if s:sub(i, i) == '[' then
                local sublist, new_i = parse_list(s, i)
                table.insert(list, sublist)
                i = new_i
            elseif s:sub(i, i) == ']' then
                return list, i + 1
            elseif s:sub(i, i) == ',' then
                i = i + 1
            else
                local j = i
                while j <= #s and s:sub(j, j):match('%d') do
                    j = j + 1
                end
                table.insert(list, tonumber(s:sub(i, j - 1)))
                i = j
            end
        end
        return list, i
    end
    
    local packet, _ = parse_list(input, 1)
    return packet
end

-- Helper function to compare two packets
local function compare_packets(left, right)
    if type(left) == 'number' and type(right) == 'number' then
        return left - right
    elseif type(left) == 'table' and type(right) == 'table' then
        local min_len = math.min(#left, #right)
        for i = 1, min_len do
            local cmp = compare_packets(left[i], right[i])
            if cmp ~= 0 then
                return cmp
            end
        end
        return #left - #right
    elseif type(left) == 'number' then
        return compare_packets({left}, right)
    elseif type(right) == 'number' then
        return compare_packets(left, {right})
    end
end

-- Main function to read input and process pairs
local function main()
    local file = io.open("input.txt", "r")
    local content = file:read("*a")
    file:close()

    local pairs = {}
    for packet1, packet2 in content:gmatch("(%b[])%s*(%b[])") do
        table.insert(pairs, {parse_packet(packet1), parse_packet(packet2)})
    end

    local sum = 0
    for i, pair in ipairs(pairs) do
        local cmp = compare_packets(pair[1], pair[2])
        if cmp < 0 then
            sum = sum + i
        end
    end

    print(sum)
end

main()
