
function sortString(w)
    local s = {}
    for i = 1, #w do
        table.insert(s, w:sub(i, i))
    end
    table.sort(s)
    return table.concat(s)
end

local file = io.open("input.txt", "r")
if not file then
    print("File reading error")
    return
end

local content = file:read("*all")
file:close()

local passphrases = {}
for line in content:gmatch("[^\r\n]+") do
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
        local sortedWord = sortString(word)
        if wordSet[sortedWord] then
            valid = false
            break
        end
        wordSet[sortedWord] = true
    end

    if valid then
        validCount = validCount + 1
    end
end

print(validCount)
