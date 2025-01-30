
local rules = {}
local myTicket = {}
local nearbyTickets = {}

-- Read input file
local lines = {}
for line in io.lines("input.txt") do
    table.insert(lines, line)
end

local section = 0
local reRule = "^(.-): (%d+)-(%d+) or (%d+)-(%d+)$"

for _, line in ipairs(lines) do
    if line == "" then
        section = section + 1
    else
        if section == 0 then
            local parts = {line:match(reRule)}
            if #parts > 1 then
                local name = parts[1]
                local ranges = {{tonumber(parts[2]), tonumber(parts[3])}, {tonumber(parts[4]), tonumber(parts[5])}}
                table.insert(rules, {name = name, ranges = ranges})
            end
        elseif section == 1 then
            if line ~= "your ticket:" then
                local ticket = {}
                for num in line:gmatch("%d+") do
                    table.insert(ticket, tonumber(num))
                end
                myTicket = ticket
            end
        elseif section == 2 then
            if line ~= "nearby tickets:" then
                local ticket = {}
                for num in line:gmatch("%d+") do
                    table.insert(ticket, tonumber(num))
                end
                nearbyTickets = nearbyTickets or {}
                local valid = true
                for _, num in ipairs(ticket) do
                    local validNum = false
                    for _, rule in ipairs(rules) do
                        for _, rng in ipairs(rule.ranges) do
                            if num >= rng[1] and num <= rng[2] then
                                validNum = true
                                break
                            end
                        end
                        if validNum then break end
                    end
                    if not validNum then
                        valid = false
                        break
                    end
                end
                if valid then
                    table.insert(nearbyTickets, ticket)
                end
            end
        end
    end
end

-- Solve field positions
local validPositions = {}
for _, rule in ipairs(rules) do
    validPositions[rule.name] = {}
    for i = 0, #nearbyTickets[1] - 1 do
        local valid = true
        for _, ticket in ipairs(nearbyTickets) do
            local value = ticket[i+1]
            local matches = false
            for _, rng in ipairs(rule.ranges) do
                if value >= rng[1] and value <= rng[2] then
                    matches = true
                    break
                end
            end
            if not matches then
                valid = false
                break
            end
        end
        if valid then
            validPositions[rule.name][i] = true
        end
    end
end

local fieldPositions = {}
repeat
    local found = false
    for name, positions in pairs(validPositions) do
        local count = 0
        local pos = -1
        for p in pairs(positions) do
            count = count + 1
            pos = p
        end
        if count == 1 then
            fieldPositions[name] = pos
            for otherName, otherPositions in pairs(validPositions) do
                otherPositions[pos] = nil
            end
            validPositions[name] = nil
            found = true
            break
        end
    end
until not found

-- Calculate departure product
local product = 1
for name, pos in pairs(fieldPositions) do
    if name:find("departure") then
        product = product * myTicket[pos + 1]
    end
end

print(product)
