
local file = io.open("input.txt", "r")
local data = file:read("*all")
file:close()

local passphrases = {}
for line in data:gmatch("[^\n]+") do
    table.insert(passphrases, line)
end

local validCount = 0

for _, passphrase in ipairs(passphrases) do
    local words = {}
    for word in passphrase:gmatch("%S+") do
        table.insert(words, word)
    end

    local wordSet = {}
    local valid = true

    for _, word in ipairs(words) do
        if wordSet[word] then
            valid = false
            break
        end
        wordSet[word] = true
    end

    if valid then
        validCount = validCount + 1
    end
end

print(validCount)
