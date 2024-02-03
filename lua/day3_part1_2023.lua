
function readFileToMatrix(filePath)
    local file = io.open(filePath, "r")
    local matrix = {}
    for line in file:lines() do
        local row = {}
        for i = 1, #line do
            table.insert(row, string.sub(line, i, i))
        end
        table.insert(matrix, row)
    end
    file:close()
    return matrix
end

function sumOfPartNumbers(matrix)
    local sum = 0
    local visited = {}
    for i = 1, #matrix do
        visited[i] = {}
        for j = 1, #matrix[i] do
            visited[i][j] = false
        end
    end

    for y = 1, #matrix do
        for x = 1, #matrix[y] do
            if not visited[y][x] and tonumber(matrix[y][x]) then
                local number, length = extractNumber(matrix, x, y)
                if isAdjacentToSymbol(matrix, x, y, length) then
                    sum = sum + number
                end
                for i = 0, length - 1 do
                    visited[y][x + i] = true
                end
            end
        end
    end
    return sum
end

function extractNumber(matrix, x, y)
    local numberStr = ""
    while x <= #matrix[y] and tonumber(matrix[y][x]) do
        numberStr = numberStr .. matrix[y][x]
        x = x + 1
    end
    return tonumber(numberStr), #numberStr
end

function isAdjacentToSymbol(matrix, x, y, length)
    for i = 0, length - 1 do
        if checkAdjacent(matrix, x + i, y) then
            return true
        end
    end
    return false
end

function checkAdjacent(matrix, x, y)
    local directions = {-1, 0, 1}
    for _, dy in ipairs(directions) do
        for _, dx in ipairs(directions) do
            local adjX, adjY = x + dx, y + dy
            if adjY >= 1 and adjY <= #matrix and adjX >= 1 and adjX <= #matrix[adjY] then
                if not tonumber(matrix[adjY][adjX]) and matrix[adjY][adjX] ~= '.' then
                    return true
                end
            end
        end
    end
    return false
end

local matrix = readFileToMatrix("input.txt")
local sum = sumOfPartNumbers(matrix)
print(sum)
