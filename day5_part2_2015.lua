
local file = io.open("input.txt", "r")
local input = file:read("*all")
file:close()

local nice = 0

local function passesRule1(line)
    for i = 1, #line - 2 do
        local toMatch = line:sub(i, i + 1)
        for j = i + 2, #line - 1 do
            if line:sub(j, j + 1) == toMatch then
                return true
            end
        end
    end
    return false
end

for line in input:gmatch("[^\n]+") do
    local rule1 = passesRule1(line)

    local rule2 = false
    for i = 1, #line - 2 do
        if line:sub(i, i) == line:sub(i + 2, i + 2) then
            rule2 = true
            break
        end
    end

    if rule1 and rule2 then
        nice = nice + 1
    end
end

print(nice)
