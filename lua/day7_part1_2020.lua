function countCanContain(target, contains)
    local seen = {}
    local function dfs(bag)
        if contains[bag] then
            for _, outer in ipairs(contains[bag]) do
                if not seen[outer] then
                    seen[outer] = true
                    dfs(outer)
                end
            end
        end
    end
    dfs(target)
    local count = 0
    for _ in pairs(seen) do count = count + 1 end
    return count
end

local contains = {}
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end
for line in file:lines() do
    local container, contained = line:match("^(.-) bags contain (.+)%.$")
    if contained ~= "no other bags" then
        for bag in contained:gmatch("(%d+ [^,]+) bag") do
            local bagName = bag:match("%d+ (.+)")
            if not contains[bagName] then contains[bagName] = {} end
            table.insert(contains[bagName], container)
        end
    end
end
file:close()

local count = countCanContain("shiny gold", contains)
print(count)