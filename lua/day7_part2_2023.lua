-- Function to determine the type of hand
function getHandType(hand)
    local counts = {}
    local jokers = 0
    for c in hand:gmatch(".") do
        if c == "J" then
            jokers = jokers + 1
        else
            counts[c] = (counts[c] or 0) + 1
        end
    end
    
    local values = {}
    for _, v in pairs(counts) do
        table.insert(values, v)
    end
    table.sort(values, function(a,b) return a > b end)
    
    if #values == 0 then  -- All jokers
        return 7
    end
    
    values[1] = values[1] + jokers
    
    if values[1] == 5 then return 7    -- Five of a kind
    elseif values[1] == 4 then return 6 -- Four of a kind
    elseif values[1] == 3 and values[2] == 2 then return 5 -- Full house
    elseif values[1] == 3 then return 4 -- Three of a kind
    elseif values[1] == 2 and values[2] == 2 then return 3 -- Two pair
    elseif values[1] == 2 then return 2 -- One pair
    else return 1 end -- High card
end

-- Function to compare hands
function compareHands(hand1, hand2)
    local order = {["A"]=14, ["K"]=13, ["Q"]=12, ["T"]=10, ["9"]=9, ["8"]=8, 
                   ["7"]=7, ["6"]=6, ["5"]=5, ["4"]=4, ["3"]=3, ["2"]=2, ["J"]=1}
    
    local type1 = getHandType(hand1)
    local type2 = getHandType(hand2)
    
    if type1 ~= type2 then
        return type1 > type2
    end
    
    for i = 1, 5 do
        local card1 = hand1:sub(i,i)
        local card2 = hand2:sub(i,i)
        if card1 ~= card2 then
            return order[card1] > order[card2]
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
table.sort(hands, function(a, b) return compareHands(a.hand, b.hand) end)

-- Calculate total winnings
local totalWinnings = 0
for rank, hand in ipairs(hands) do
    totalWinnings = totalWinnings + (hand.bid * (#hands - rank + 1))
end

-- Print result
print(totalWinnings)
