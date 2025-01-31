
local md5 = require("md5")

local file = io.open("input.txt", "r")
local doorID = file:read("*l")
file:close()
doorID = string.gsub(doorID, "^%s*(.-)%s*$", "%1")

local password = {}
local filledPositions = 0
local found = {}
for i = 0, 7 do
    found[i] = false
end

local i = 0
while filledPositions < 8 do
    local hash = md5.sumhexa(doorID .. tostring(i))
    if string.sub(hash, 1, 5) == "00000" then
        local pos = string.sub(hash, 6, 6)
        if pos >= '0' and pos <= '7' then
            local posIndex = tonumber(pos)
            if not found[posIndex] then
                found[posIndex] = true
                password[posIndex + 1] = string.sub(hash, 7, 7)
                filledPositions = filledPositions + 1
            end
        end
    end
    i = i + 1
end

print(table.concat(password))
