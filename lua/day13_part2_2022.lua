
local function parse_packet(s)
    local stack = {}
    local current = {}
    local i = 1
    while i <= #s do
        local char = s:sub(i, i)
        if char == '[' then
            table.insert(stack, current)
            current = {}
        elseif char == ']' then
            local parent = table.remove(stack)
            table.insert(parent, current)
            current = parent
        elseif char == ',' then
            -- do nothing
        else
            local num_str = ""
            while i <= #s and s:sub(i, i):match("%d") do
                num_str = num_str .. s:sub(i, i)
                i = i + 1
            end
            table.insert(current, tonumber(num_str))
            i = i - 1
        end
        i = i + 1
    end
    return current[1]
end

local function compare(left, right)
    if type(left) == "number" and type(right) == "number" then
        if left < right then
            return true
        elseif left > right then
            return false
        else
            return nil
        end
    elseif type(left) == "table" and type(right) == "table" then
        local i = 1
        while true do
            local lval = left[i]
            local rval = right[i]
            if lval == nil and rval == nil then
                return nil
            elseif lval == nil then
                return true
            elseif rval == nil then
                return false
            else
                local result = compare(lval, rval)
                if result ~= nil then
                    return result
                end
            end
            i = i + 1
        end
    elseif type(left) == "number" then
        return compare({left}, right)
    else
        return compare(left, {right})
    end
end

local function solve()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening input.txt")
        return
    end

    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    local part1_sum = 0
    local all_packets = {}
    local pair_index = 1
    for i = 1, #lines, 3 do
        if lines[i] and lines[i+1] then
            local left = parse_packet(lines[i])
            local right = parse_packet(lines[i+1])
            table.insert(all_packets, left)
            table.insert(all_packets, right)
            if compare(left, right) then
                part1_sum = part1_sum + pair_index
            end
            pair_index = pair_index + 1
        end
    end

    print("Part 1:", part1_sum)

    local divider1 = parse_packet("[[2]]")
    local divider2 = parse_packet("[[6]]")
    table.insert(all_packets, divider1)
    table.insert(all_packets, divider2)

    table.sort(all_packets, function(a, b)
        return compare(a, b)
    end)

    local decoder_key = 1
    local divider1_index = 0
    local divider2_index = 0
    for i, packet in ipairs(all_packets) do
        if packet == divider1 then
            divider1_index = i
        elseif packet == divider2 then
            divider2_index = i
        end
    end
    decoder_key = divider1_index * divider2_index
    print("Part 2:", decoder_key)
end

solve()
