-- Function to determine the type of a hand
local function getHandType(hand)
    local counts = {}
    for i = 1, #hand do
        counts[hand:sub(i,i)] = (counts[hand:sub(i,i)] or 0) + 1
    end
    
    local values = {}
    for _, v in pairs(counts) do
        table.insert(values, v)
    end
    table.sort(values, function(a,b) return a > b end)
    
    if values[1] == 5 then return 7 -- Five of a kind
    elseif values[1] == 4 then return 6 -- Four of a kind
    elseif values[1] == 3 and values[2] == 2 then return 5 -- Full house
    elseif values[1] == 3 then return 4 -- Three of a kind
    elseif values[1] == 2 and values[2] == 2 then return 3 -- Two pair
    elseif values[1] == 2 then return 2 -- One pair
    else return 1 -- High card
    end
end

-- Function to compare two hands
local function compareHands(hand1, hand2)
    local cardOrder = {['A'] = 14, ['K'] = 13, ['Q'] = 12, ['J'] = 11, ['T'] = 10}
    for i = 2, 9 do cardOrder[tostring(i)] = i end
    
    local type1 = getHandType(hand1)
    local type2 = getHandType(hand2)
    
    if type1 ~= type2 then
        return type1 > type2
    else
        for i = 1, 5 do
            local card1 = cardOrder[hand1:sub(i,i)]
            local card2 = cardOrder[hand2:sub(i,i)]
            if card1 ~= card2 then
                return card1 > card2
            end
        end
    end
    return false
end

-- Read input
local hands = {}
for line in io.lines("input.txt") do
    local hand, bid = line:match("(%S+)%s+(%d+)")
    table.insert(hands, {hand = hand, bid = tonumber(bid)})
end

-- Sort hands
table.sort(hands, function(a, b) return compareHands(b.hand, a.hand) end)

-- Calculate total winnings
local totalWinnings = 0
for rank, hand in ipairs(hands) do
    totalWinnings = totalWinnings + rank * hand.bid
end

-- Print result
print(totalWinnings)
