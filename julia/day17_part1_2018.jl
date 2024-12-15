
function solve()
    input = read("input.txt", String)
    lines = split(strip(input), "\n")

    ground = [['+']]
    maxX, minX, maxY, minY = 0, 0, 0, 20
    xOffset, yOffset = 500, 0

    for line in lines
        split_line = split(line, r"[=, .]+")
        if split_line[1] == "x"
            x = parse(Int, split_line[2]) - xOffset
            y1 = parse(Int, split_line[4]) - yOffset
            y2 = parse(Int, split_line[5]) - yOffset

            while x >= maxX
                maxX += 1
                for j in eachindex(ground)
                    push!(ground[j], '.')
                end
            end
            while x <= minX
                minX -= 1
                for j in eachindex(ground)
                    ground[j] = ['.', ground[j]...]
                end
            end
            while y2 > maxY
                maxY += 1
                push!(ground, fill('.', length(ground[1])))
            end
            minY = min(minY, y1)
            for i in y1:y2
                ground[i][x-minX+1] = '#'
            end
        else
            y = parse(Int, split_line[2]) - yOffset
            x1 = parse(Int, split_line[4]) - xOffset
            x2 = parse(Int, split_line[5]) - xOffset

            while y > maxY
                maxY += 1
                push!(ground, fill('.', length(ground[1])))
            end
            while x2 >= maxX
                maxX += 1
                for j in eachindex(ground)
                    push!(ground[j], '.')
                end
            end
            while x1 <= minX
                minX -= 1
                for j in eachindex(ground)
                    ground[j] = ['.', ground[j]...]
                end
            end
            minY = min(minY, y)
            for i in x1:x2
                ground[y][i-minX+1] = '#'
            end
        end
    end

    waterCount = 0
    flowCount = 0
    roundLimit = 200000

    while ground[2][-minX+1] != '|' && waterCount < roundLimit
        canMove = true
        x = -minX + 1
        y = 2
        tryLeft = 0
        while canMove
            if y+1 > maxY || ground[y+1][x] == '|'
                ground[y][x] = '|'
                canMove = false
                if y >= minY
                    flowCount += 1
                end
            elseif ground[y+1][x] == '.'
                y += 1
                tryLeft = 0
            elseif ground[y+1][x] == '#' || ground[y+1][x] == '~'
                if (tryLeft == 1 && ground[y][x-1] == '|') ||
                   (tryLeft == 2 && ground[y][x+1] == '|') ||
                   (ground[y][x+1] == '|' && ground[y][x-1] != '.') ||
                   (ground[y][x+1] != '.' && ground[y][x-1] == '|')
                    ground[y][x] = '|'
                    flowCount += 1
                    canMove = false
                    i = x + 1
                    while i <= length(ground[y]) && ground[y][i] == '~'
                        ground[y][i] = '|'
                        waterCount -= 1
                        flowCount += 1
                        i += 1
                    end
                    i = x - 1
                    while i >= 1 && ground[y][i] == '~'
                        ground[y][i] = '|'
                        waterCount -= 1
                        flowCount += 1
                        i -= 1
                    end
                elseif (tryLeft == 0 && ground[y][x-1] == '.') ||
                       (tryLeft == 1 && ground[y][x-1] == '.')
                    x -= 1
                    tryLeft = 1
                elseif (tryLeft == 0 && ground[y][x+1] == '.') ||
                       (tryLeft == 2 && ground[y][x+1] == '.')
                    x += 1
                    tryLeft = 2
                else
                    canMove = false
                    ground[y][x] = '~'
                    waterCount += 1
                end
            end
        end
    end
    println(flowCount + waterCount)
end

solve()
