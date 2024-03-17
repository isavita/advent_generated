local function read_input()
    local file = io.open("input.txt", "r")
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

local function play_combat(deck1, deck2)
    local round = 1
    local seen = {}
    while #deck1 > 0 and #deck2 > 0 do
        local key = table.concat(deck1, ",") .. ":" .. table.concat(deck2, ",")
        if seen[key] then
            return 1, deck1
        end
        seen[key] = true
        local card1 = table.remove(deck1, 1)
        local card2 = table.remove(deck2, 1)
        local winner
        if #deck1 >= card1 and #deck2 >= card2 then
            local subdeck1 = {}
            for i = 1, card1 do
                table.insert(subdeck1, deck1[i])
            end
            local subdeck2 = {}
            for i = 1, card2 do
                table.insert(subdeck2, deck2[i])
            end
            winner, _ = play_combat(subdeck1, subdeck2)
        elseif card1 > card2 then
            winner = 1
        else
            winner = 2
        end
        if winner == 1 then
            table.insert(deck1, card1)
            table.insert(deck1, card2)
        else
            table.insert(deck2, card2)
            table.insert(deck2, card1)
        end
        round = round + 1
    end
    if #deck1 > 0 then
        return 1, deck1
    else
        return 2, deck2
    end
end

local function calculate_score(deck)
    local score = 0
    for i, card in ipairs(deck) do
        score = score + card * (#deck - i + 1)
    end
    return score
end

local lines = read_input()
local deck1, deck2 = {}, {}
local player = 1
for _, line in ipairs(lines) do
    if line:match("Player %d:") then
        player = tonumber(line:match("%d"))
    elseif line ~= "" then
        if player == 1 then
            table.insert(deck1, tonumber(line))
        else
            table.insert(deck2, tonumber(line))
        end
    end
end

local winner, winning_deck = play_combat(deck1, deck2)
local score = calculate_score(winning_deck)
print(score)