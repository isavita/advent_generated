
function readInput(filename)
    local file = io.open(filename, "r")
    local currentPassword = file:read("*line")
    file:close()
    return currentPassword
end

function findNextPassword(password)
    repeat
        password = incrementPassword(password)
    until isValidPassword(password)
    return password
end

function incrementPassword(password)
    local chars = {}
    password:gsub(".", function(c) table.insert(chars, c) end)
    for i = #chars, 1, -1 do
        chars[i] = string.char(chars[i]:byte() + 1)
        if chars[i] > 'z' then
            chars[i] = 'a'
        else
            break
        end
    end
    return table.concat(chars)
end

function isValidPassword(password)
    return hasStraight(password) and not containsInvalidLetters(password) and hasTwoPairs(password)
end

function hasStraight(password)
    for i = 1, #password - 2 do
        if password:byte(i) + 1 == password:byte(i + 1) and password:byte(i) + 2 == password:byte(i + 2) then
            return true
        end
    end
    return false
end

function containsInvalidLetters(password)
    return password:find("[iol]") ~= nil
end

function hasTwoPairs(password)
    local count = 0
    local i = 1
    while i <= #password - 1 do
        if password:byte(i) == password:byte(i + 1) then
            count = count + 1
            i = i + 2
        else
            i = i + 1
        end
    end
    return count >= 2
end

local currentPassword = readInput("input.txt")
local firstNewPassword = findNextPassword(currentPassword)
local secondNewPassword = findNextPassword(firstNewPassword)
print(secondNewPassword)
