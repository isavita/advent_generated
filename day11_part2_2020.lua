
local directions = {
    {x = -1, y = -1}, {x = 0, y = -1}, {x = 1, y = -1},
    {x = -1, y = 0}, {x = 1, y = 0},
    {x = -1, y = 1}, {x = 0, y = 1}, {x = 1, y = 1}
}

function readInput(filename)
    local file = io.open(filename, "r")
    local seatingArea = {}
    for line in file:lines() do
        local row = {}
        for i = 1, #line do
            table.insert(row, line:sub(i, i))
        end
        table.insert(seatingArea, row)
    end
    file:close()
    return seatingArea
end

function simulateSeatingPartTwo(seatingArea)
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
            if seatingArea[i][j] == 'L' then
                if countVisibleOccupied(seatingArea, i, j) == 0 then
                    newSeatingArea[i][j] = '#'
                    stabilized = false
                end
            elseif seatingArea[i][j] == '#' then
                if countVisibleOccupied(seatingArea, i, j) >= 5 then
                    newSeatingArea[i][j] = 'L'
                    stabilized = false
                end
            end
        end
    end

    return newSeatingArea, stabilized
end

function countVisibleOccupied(seatingArea, row, col)
    local count = 0
    for _, dir in ipairs(directions) do
        local r, c = row + dir.y, col + dir.x
        while r >= 1 and r <= #seatingArea and c >= 1 and c <= #seatingArea[1] do
            if seatingArea[r][c] == 'L' then
                break
            elseif seatingArea[r][c] == '#' then
                count = count + 1
                break
            end
            r = r + dir.y
            c = c + dir.x
        end
    end
    return count
end

function countOccupiedSeats(seatingArea)
    local count = 0
    for i = 1, #seatingArea do
        for j = 1, #seatingArea[1] do
            if seatingArea[i][j] == '#' then
                count = count + 1
            end
        end
    end
    return count
end

local seatingArea = readInput("input.txt")
local stabilized = false
repeat
    seatingArea, stabilized = simulateSeatingPartTwo(seatingArea)
until stabilized

print(countOccupiedSeats(seatingArea))
