local function isNice(line)
    local vowels = 0
    for char in line:gmatch(".") do
        if char:match("[aeiou]") then
            vowels = vowels + 1
        end
    end

    local hasDouble = false
    local prevChar = nil
    for char in line:gmatch(".") do
        if char == prevChar then
            hasDouble = true
            break
        end
        prevChar = char
    end

    local hasDisallowed = line:match("ab") or line:match("cd") or line:match("pq") or line:match("xy")

    return vowels >= 3 and not hasDisallowed and hasDouble
end

local function countNiceLines(filename)
    local file = io.open(filename, "r")
    local niceCount = 0
    for line in file:lines() do
        if isNice(line) then
            niceCount = niceCount + 1
        end
    end
    file:close()
    return niceCount
end

local niceLines = countNiceLines("input.txt")
print(niceLines)