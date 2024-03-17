-- Read the input from the "input.txt" file
local file = io.open("input.txt", "r")
local line = file:read("*l")
file:close()

-- Parse the input
local players, lastMarble = line:match("(%d+) players; last marble is worth (%d+) points")
players = tonumber(players)
lastMarble = tonumber(lastMarble)

-- Multiply the last marble number by 100
lastMarble = lastMarble * 100

-- Implement the marble game
local scores = {}
for i = 1, players do
    scores[i] = 0
end

local current = {value = 0, prev = nil, next = nil}
current.prev = current
current.next = current

for marble = 1, lastMarble do
    if marble % 23 == 0 then
        local player = (marble - 1) % players + 1
        for i = 1, 7 do
            current = current.prev
        end
        scores[player] = scores[player] + marble + current.value
        current.prev.next = current.next
        current.next.prev = current.prev
        current = current.next
    else
        local newMarble = {value = marble, prev = current.next, next = current.next.next}
        current.next.next.prev = newMarble
        current.next.next = newMarble
        current = newMarble
    end
end

-- Find the maximum score
local maxScore = 0
for _, score in ipairs(scores) do
    if score > maxScore then
        maxScore = score
    end
end

-- Print the answer
print(maxScore)