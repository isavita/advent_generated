
local function readInput(filename)
    local orderingRules, updates = {}, {}
    local isUpdateSection = false
    local file = io.open(filename, "r")
    if not file then return nil, nil, "Error opening file" end

    for line in file:lines() do
        line = line:match("^%s*(.-)%s*$")
        if line == "" then
            isUpdateSection = true
        elseif not isUpdateSection then
            local x, y = line:match("^(%d+)%s*|%s*(%d+)$")
            if x and y then
                table.insert(orderingRules, {tonumber(x), tonumber(y)})
            end
        else
            local update = {}
            for numStr in line:gmatch("([^,]+)") do
                local num = tonumber(numStr:match("^%s*(%d+)%s*$"))
                if num then table.insert(update, num) end
            end
            if #update > 0 then table.insert(updates, update) end
        end
    end
    file:close()
    return orderingRules, updates, nil
end

local function isCorrectlyOrdered(update, rules)
    local position = {}
    for idx, page in ipairs(update) do
        position[page] = idx
    end
    for _, rule in ipairs(rules) do
        local x, y = rule[1], rule[2]
        local posX, posY = position[x], position[y]
        if posX and posY and posX >= posY then
            return false
        end
    end
    return true
end

local orderingRules, updates, err = readInput("input.txt")
if err then
    print("Error reading input:", err)
    os.exit(1)
end

local sum = 0
for _, update in ipairs(updates) do
    if isCorrectlyOrdered(update, orderingRules) then
        sum = sum + update[math.floor(#update/2) + 1]
    end
end

print(sum)
