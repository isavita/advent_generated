
function readFirstRow(filename)
    file = open(filename)
    firstRow = readline(file)
    close(file)
    return firstRow
end

function countSafeTiles(firstRow, totalRows)
    currentRow = firstRow
    safeCount = countChar(currentRow, '.')

    for i in 1:totalRows-1
        nextRow = ""
        for j in 1:length(currentRow)
            if isTrap(j-1, j, j+1, currentRow)
                nextRow *= "^"
            else
                nextRow *= "."
                safeCount += 1
            end
        end
        currentRow = nextRow
    end
    return safeCount
end

function isTrap(left, center, right, row)
    l = safeIfOutOfBounds(left, row)
    c = row[center]
    r = safeIfOutOfBounds(right, row)

    return (l == '^' && c == '^' && r == '.') ||
           (c == '^' && r == '^' && l == '.') ||
           (l == '^' && c == '.' && r == '.') ||
           (r == '^' && c == '.' && l == '.')
end

function safeIfOutOfBounds(index, row)
    if index < 1 || index > length(row)
        return '.'
    end
    return row[index]
end

function countChar(str, char)
    count = 0
    for c in str
        if c == char
            count += 1
        end
    end
    return count
end

firstRow = readFirstRow("input.txt")
totalRows = 40
safeTilesCount = countSafeTiles(firstRow, totalRows)
println(safeTilesCount)
