
function solve_water_problem()
    input = read("input.txt", String) |> strip
    lines = split(input, '\n')

    ground = Dict{Tuple{Int,Int}, Char}([(0,0) => '+'])
    maxX, minX, maxY, minY = 0, 0, 0, 20
    xOffset, yOffset = 500, 0

    for line in lines
        parts = split(line, r"[=, .]+")
        if parts[1] == "x"
            x = parse(Int, parts[2]) - xOffset
            y1 = parse(Int, parts[4]) - yOffset
            y2 = parse(Int, parts[5]) - yOffset

            for y in y1:y2
                ground[(x,y)] = '#'
                maxY = max(maxY, y)
                minY = min(minY, y)
            end
            minX = min(minX, x)
            maxX = max(maxX, x)
        else
            y = parse(Int, parts[2]) - yOffset
            x1 = parse(Int, parts[4]) - xOffset
            x2 = parse(Int, parts[5]) - xOffset

            for x in x1:x2
                ground[(x,y)] = '#'
                maxX = max(maxX, x)
                minX = min(minX, x)
            end
            minY = min(minY, y)
            maxY = max(maxY, y)
        end
    end

    waterCount = 0
    flowCount = 0
    roundLimit = 200000

    for round in 1:roundLimit
        canMove = true
        x, y = 0, 1
        tryLeft = 0

        while canMove
            if y > maxY || get(ground, (x,y+1), '.') == '|'
                ground[(x,y)] = '|'
                canMove = false
                y >= minY && (flowCount += 1)
                break
            elseif get(ground, (x,y+1), '.') == '.'
                y += 1
                tryLeft = 0
            elseif get(ground, (x,y+1), '.') in ['#', '~']
                if ((tryLeft == 1 && get(ground, (x-1,y), '.') == '|') ||
                    (tryLeft == 2 && get(ground, (x+1,y), '.') == '|') ||
                    (get(ground, (x+1,y), '.') == '|' && get(ground, (x-1,y), '.') != '.') ||
                    (get(ground, (x+1,y), '.') != '.' && get(ground, (x-1,y), '.') == '|'))
                    ground[(x,y)] = '|'
                    flowCount += 1
                    canMove = false

                    for i in x+1:maxX
                        get(ground, (i,y), '.') == '~' || break
                        ground[(i,y)] = '|'
                        waterCount -= 1
                        flowCount += 1
                    end

                    for i in x-1:-1:minX
                        get(ground, (i,y), '.') == '~' || break
                        ground[(i,y)] = '|'
                        waterCount -= 1
                        flowCount += 1
                    end
                    break
                elseif ((tryLeft == 0 && get(ground, (x-1,y), '.') == '.') ||
                        (tryLeft == 1 && get(ground, (x-1,y), '.') == '.'))
                    x -= 1
                    tryLeft = 1
                elseif ((tryLeft == 0 && get(ground, (x+1,y), '.') == '.') ||
                        (tryLeft == 2 && get(ground, (x+1,y), '.') == '.'))
                    x += 1
                    tryLeft = 2
                else
                    canMove = false
                    ground[(x,y)] = '~'
                    waterCount += 1
                end
            end
        end

        get(ground, (0,1), '.') == '|' && break
    end

    println(waterCount)
end

solve_water_problem()
