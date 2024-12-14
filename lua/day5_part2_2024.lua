
local function readInput(filename)
    local file = io.open(filename, "r")
    if not file then return nil, nil, "Error opening file" end
    local orderingRules, updates, isUpdateSection = {}, {}, false
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
    for idx, page in ipairs(update) do position[page] = idx end
    for _, rule in ipairs(rules) do
        local x, y = rule[1], rule[2]
        local posX, posY = position[x], position[y]
        if posX and posY and posX >= posY then return false end
    end
    return true
end

local function sortUpdate(update, rules)
    local adjacency, pagesInUpdate = {}, {}
    for _, page in ipairs(update) do
        pagesInUpdate[page] = true
        adjacency[page] = {}
    end
    for _, rule in ipairs(rules) do
        local x, y = rule[1], rule[2]
        if pagesInUpdate[x] and pagesInUpdate[y] then
            table.insert(adjacency[x], y)
        end
    end
    local visited, tempMarked, result = {}, {}, {}
    local function visit(n)
        if tempMarked[n] then return "Cycle detected" end
        if not visited[n] then
            tempMarked[n] = true
            for _, m in ipairs(adjacency[n]) do
                local err = visit(m)
                if err then return err end
            end
            tempMarked[n] = false
            visited[n] = true
            table.insert(result, n)
        end
    end
    for page in pairs(pagesInUpdate) do
        if not visited[page] then
            local err = visit(page)
            if err then return nil, err end
        end
    end
    for i = 1, #result / 2 do
        result[i], result[#result - i + 1] = result[#result - i + 1], result[i]
    end
    return result, nil
end

local orderingRules, updates, err = readInput("input.txt")
if err then print("Error reading input:", err) return end

local sum = 0
for _, update in ipairs(updates) do
    if not isCorrectlyOrdered(update, orderingRules) then
        local sortedUpdate, err = sortUpdate(update, orderingRules)
        if err then print("Error sorting update:", err) else
            sum = sum + sortedUpdate[math.floor(#sortedUpdate / 2) + 1]
        end
    end
end
print(sum)
