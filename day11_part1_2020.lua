
local seatingArea = {}
for line in io.lines("input.txt") do
    table.insert(seatingArea, {string.byte(line, 1, -1)})
end

local function simulateSeating(seatingArea)
    local rows = #seatingArea
    local cols = #seatingArea[1]
    local newSeatingArea = {}
    for i = 1, rows do
        newSeatingArea[i] = {}
        for j = 1, cols do
            newSeatingArea[i][j] = seatingArea[i][j]
        end
    end
    local stabilized = true

    for i = 1, rows do
        for j = 1, cols do
            local count = 0
            for x = -1, 1 do
                for y = -1, 1 do
                    if not (x == 0 and y == 0) and i + x >= 1 and i + x <= rows and j + y >= 1 and j + y <= cols then
                        if seatingArea[i + x][j + y] == string.byte('#') then
                            count = count + 1
                        end
                    end
                end
            end

            if seatingArea[i][j] == string.byte('L') and count == 0 then
                newSeatingArea[i][j] = string.byte('#')
                stabilized = false
            elseif seatingArea[i][j] == string.byte('#') and count >= 4 then
                newSeatingArea[i][j] = string.byte('L')
                stabilized = false
            end
        end
    end

    return newSeatingArea, stabilized
end

local function countOccupiedSeats(seatingArea)
    local count = 0
    for i = 1, #seatingArea do
        for j = 1, #seatingArea[1] do
            if seatingArea[i][j] == string.byte('#') then
                count = count + 1
            end
        end
    end
    return count
end

local stabilized = false
repeat
    seatingArea, stabilized = simulateSeating(seatingArea)
until stabilized

print(countOccupiedSeats(seatingArea))
