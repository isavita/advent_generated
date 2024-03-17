local function get_points_for_card(card)
    local points = 0
    for given, count in pairs(card.givens) do
        if card.winnings[given] then
            points = points + count * card.winnings[given]
        end
    end
    return points
end

local function lex_line_into_card(line)
    local _, _, card_data_str = string.find(line, ": (.*)")
    if not card_data_str then
        return nil
    end

    local card_data = {}
    for part in string.gmatch(card_data_str, "[^%|]+") do
        table.insert(card_data, part)
    end

    local winnings = {}
    for point in string.gmatch(card_data[1], "%d+") do
        winnings[tonumber(point)] = (winnings[tonumber(point)] or 0) + 1
    end

    local givens = {}
    for point in string.gmatch(card_data[2], "%d+") do
        givens[tonumber(point)] = (givens[tonumber(point)] or 0) + 1
    end

    return {
        winnings = winnings,
        givens = givens,
        total_count = 1
    }
end

local file = io.open("input.txt", "r")
local input = file:read("*a")
file:close()

local cards = {}
for line in string.gmatch(input, "[^\n]+") do
    if #line > 0 then
        local card = lex_line_into_card(line)
        if card then
            table.insert(cards, card)
        end
    end
end

for i, card in ipairs(cards) do
    local points = get_points_for_card(card)
    for j = 1, points do
        if i + j <= #cards then
            cards[i + j].total_count = cards[i + j].total_count + 1 * card.total_count
        end
    end
end

local total_cards = 0
for _, card in ipairs(cards) do
    total_cards = total_cards + card.total_count
end

print(total_cards)