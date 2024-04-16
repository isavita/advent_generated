function isRealRoom(room)
    local parts = {}
    for part in room:gmatch("[^-]+") do
        table.insert(parts, part)
    end
    local checksum = parts[#parts]:match("%[(.*)%]")
    parts[#parts] = parts[#parts]:match("^(%d+)")

    local encryptedName = {}
    for i = 1, #parts - 1 do
        table.insert(encryptedName, parts[i])
    end

    local letterCounts = {}
    for _, part in ipairs(encryptedName) do
        for letter in part:gmatch(".") do
            if not letterCounts[letter] then
                letterCounts[letter] = 0
            end
            letterCounts[letter] = letterCounts[letter] + 1
        end
    end

    local sortedLetters = {}
    for letter, count in pairs(letterCounts) do
        table.insert(sortedLetters, {letter = letter, count = count})
    end
    table.sort(sortedLetters, function(a, b)
        if a.count == b.count then
            return a.letter < b.letter
        else
            return a.count > b.count
        end
    end)

    for i = 1, #checksum do
        if checksum:sub(i, i) ~= sortedLetters[i].letter then
            return false
        end
    end

    return true
end

function getSectorID(room)
    local sectorIDPart = room:match("(%d+)%[")
    return tonumber(sectorIDPart)
end

function decryptName(room)
    local parts = {}
    for part in room:gmatch("[^-]+") do
        table.insert(parts, part)
    end
    local sectorID = tonumber(parts[#parts]:match("(%d+)"))
    parts[#parts] = nil

    local decryptedName = {}
    for _, part in ipairs(parts) do
        local decryptedPart = {}
        for letter in part:gmatch(".") do
            local shiftedLetter = string.char(((letter:byte() - string.byte('a') + sectorID) % 26) + string.byte('a'))
            table.insert(decryptedPart, shiftedLetter)
        end
        table.insert(decryptedName, table.concat(decryptedPart))
    end

    return table.concat(decryptedName, " ")
end

local file = io.open("input.txt", "r")
if not file then
    error("Could not open file")
end

for line in file:lines() do
    if isRealRoom(line) then
        local decryptedName = decryptName(line)
        if decryptedName:find("northpole object") then
            print(getSectorID(line))
            break
        end
    end
end

file:close()