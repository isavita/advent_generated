local Size = 10007

local function dealIntoNewStack(deck)
    for i = 0, math.floor(Size / 2) - 1 do
        deck[i + 1], deck[Size - i] = deck[Size - i], deck[i + 1]
    end
    return deck
end

local function cutN(deck, n)
    if n >= 0 then
        local temp = {}
        for i = 1, Size do
            temp[i] = deck[(i + n - 1) % Size + 1]
        end
        return temp
    else
        local temp = {}
        for i = 1, Size do
            temp[i] = deck[((i + Size + n - 1) % Size) + 1]
        end
        return temp
    end
end

local function dealWithIncrement(deck, n)
    local newDeck = {}
    for i = 0, Size - 1 do
        newDeck[(i * n) % Size + 1] = deck[i + 1]
    end
    return newDeck
end

local function find2019(deck)
    for i = 1, Size do
        if deck[i] == 2019 then
            return i - 1
        end
    end
    return -1
end

local deck = {}
for i = 0, Size - 1 do
    deck[i + 1] = i
end

local file = io.open("input.txt", "r")
for line in file:lines() do
    if line == "deal into new stack" then
        deck = dealIntoNewStack(deck)
    elseif line:sub(1, 3) == "cut" then
        local n = tonumber(line:match("%-?%d+"))
        deck = cutN(deck, n)
    elseif line:sub(1, 19) == "deal with increment" then
        local n = tonumber(line:match("%-?%d+$"))
        deck = dealWithIncrement(deck, n)
    end
end
file:close()

print(find2019(deck))