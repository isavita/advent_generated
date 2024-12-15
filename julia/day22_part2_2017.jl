
function solve()
    input = read("input.txt", String)
    grid = Dict{Tuple{Int, Int}, Char}()
    lines = split(strip(input), "\n")
    rows = length(lines)
    cols = length(lines[1])
    
    for (r, line) in enumerate(lines)
        for (c, char) in enumerate(line)
            grid[(r - (rows + 1) ÷ 2, c - (cols + 1) ÷ 2)] = char
        end
    end

    function part1(grid)
        pos = (0, 0)
        dir = (-1, 0) # Up
        infections = 0
        for _ in 1:10000
            if get(grid, pos, '.') == '#'
                dir = (dir[2], -dir[1]) # Turn right
                grid[pos] = '.'
            else
                dir = (-dir[2], dir[1]) # Turn left
                grid[pos] = '#'
                infections += 1
            end
            pos = (pos[1] + dir[1], pos[2] + dir[2])
        end
        return infections
    end

    function part2(grid)
        pos = (0, 0)
        dir = (-1, 0) # Up
        infections = 0
        for _ in 1:10000000
            state = get(grid, pos, '.')
            if state == '.'
                dir = (-dir[2], dir[1]) # Turn left
                grid[pos] = 'W'
            elseif state == 'W'
                grid[pos] = '#'
                infections += 1
            elseif state == '#'
                dir = (dir[2], -dir[1]) # Turn right
                grid[pos] = 'F'
            elseif state == 'F'
                dir = (-dir[1], -dir[2]) # Reverse
                grid[pos] = '.'
            end
            pos = (pos[1] + dir[1], pos[2] + dir[2])
        end
        return infections
    end

    println(part1(deepcopy(grid)))
    println(part2(deepcopy(grid)))
end

solve()
