local file = io.open("input.txt", "r")
if not file then
    error("Failed to open input.txt")
end

local player1_deck, player2_deck = {}, {}
local current_deck = player1_deck

for line in file:lines() do
    if line == "" then
        current_deck = player2_deck
    elseif not line:find("Player") then
        table.insert(current_deck, tonumber(line))
    end
end

file:close()

while #player1_deck > 0 and #player2_deck > 0 do
    local card1, card2 = table.remove(player1_deck, 1), table.remove(player2_deck, 1)
    if card1 > card2 then
        table.insert(player1_deck, card1)
        table.insert(player1_deck, card2)
    else
        table.insert(player2_deck, card2)
        table.insert(player2_deck, card1)
    end
end

local winning_deck = #player1_deck > 0 and player1_deck or player2_deck
local score = 0
for i, card in ipairs(winning_deck) do
    score = score + card * (#winning_deck - i + 1)
end

print(score)