-- Read input from "input.txt" file
local file = io.open("input.txt", "r")
local line = file:read("*line")
file:close()

local players, lastMarble = line:match("(%d+) players; last marble is worth (%d+) points")
players = tonumber(players)
lastMarble = tonumber(lastMarble)

-- Marble structure
local Marble = {
    value = 0,
    prev = nil,
    next = nil
}

Marble.__index = Marble

function Marble:new()
    local obj = setmetatable({}, Marble)
    return obj
end

function playMarbleGame(players, lastMarble)
    local scores = {}
    for i = 1, players do
        scores[i] = 0
    end

    local current = Marble:new()
    current.next = current
    current.prev = current

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
            current = current.next
            local newMarble = Marble:new()
            newMarble.value = marble
            newMarble.prev = current
            newMarble.next = current.next
            current.next.prev = newMarble
            current.next = newMarble
            current = newMarble
        end
    end

    local maxScore = 0
    for _, score in ipairs(scores) do
        if score > maxScore then
            maxScore = score
        end
    end
    return maxScore
end

local maxScore = playMarbleGame(players, lastMarble)
print(maxScore)