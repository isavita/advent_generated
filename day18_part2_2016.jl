
function readFirstRow(filename::String)
    f = open(filename)
    firstRow = readline(f)
    close(f)
    return firstRow
end

function countSafeTiles(firstRow::String, totalRows::Int)
    currentRow = firstRow
    safeCount = countChar(currentRow, '.')

    for i in 2:totalRows
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

function isTrap(left::Int, center::Int, right::Int, row::String)
    l = safeIfOutOfBounds(left, row)
    c = row[center]
    r = safeIfOutOfBounds(right, row)

    return (l == '^' && c == '^' && r == '.') ||
           (c == '^' && r == '^' && l == '.') ||
           (l == '^' && c == '.' && r == '.') ||
           (r == '^' && c == '.' && l == '.')
end

function safeIfOutOfBounds(index::Int, row::String)
    if index < 1 || index > length(row)
        return '.'
    end
    return row[index]
end

function countChar(str::String, char::Char)
    count = 0
    for c in str
        if c == char
            count += 1
        end
    end
    return count
end

firstRow = readFirstRow("input.txt")
totalRows = 400000
safeTilesCount = countSafeTiles(firstRow, totalRows)
println(safeTilesCount)
