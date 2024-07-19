
local md5 = require("md5")

local function findPassword(doorID)
    local password = {}
    local i = 0
    while #password < 8 do
        local hash = md5.sumhexa(doorID .. i)
        if hash:sub(1, 5) == "00000" then
            password[#password + 1] = hash:sub(6, 6)
        end
        i = i + 1
    end
    return table.concat(password)
end

local function main()
    local file = io.open("input.txt", "r")
    local doorID = file:read("*a"):gsub("%s+", "")
    file:close()
    local password = findPassword(doorID)
    print(password)
end

main()
