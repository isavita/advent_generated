
local deps = {}
local allSteps = {}

local file = io.open("input.txt", "r")
if not file then
    error("Error opening file")
end

for line in file:lines() do
    local a, b = string.match(line, "Step (%a) must be finished before step (%a) can begin.")
    if not deps[b] then
        deps[b] = {}
    end
    table.insert(deps[b], a)
    allSteps[a] = true
    allSteps[b] = true
end
file:close()

local function contains(slice, val)
    for _, item in ipairs(slice) do
        if item == val then
            return true
        end
    end
    return false
end

local function remove(slice, val)
    for i, item in ipairs(slice) do
        if item == val then
            table.remove(slice, i)
            return slice
        end
    end
    return slice
end

local order = {}
local available = {}

for step, _ in pairs(allSteps) do
    if not deps[step] or #deps[step] == 0 then
        table.insert(available, step)
    end
end
table.sort(available)

while #available > 0 do
    local next = table.remove(available, 1)
    table.insert(order, next)

    for step, _ in pairs(allSteps) do
        if contains(deps[step] or {}, next) then
            deps[step] = remove(deps[step] or {}, next)
            if not deps[step] or #deps[step] == 0 then
                table.insert(available, step)
            end
        end
    end
    table.sort(available)
end

print(table.concat(order))
