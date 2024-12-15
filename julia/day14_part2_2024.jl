
function mod(a, b)
    (a % b + b) % b
end

function parse_line(line)
    m = match(r"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)", line)
    if m === nothing
        error("Invalid line format: $line")
    end
    x, y, vx, vy = parse.(Int, m.captures)
    return (x, y, vx, vy)
end

function move_robots!(robots, size_x, size_y)
    for i in eachindex(robots)
        x, y, vx, vy = robots[i]
        robots[i] = (mod(x + vx, size_x), mod(y + vy, size_y), vx, vy)
    end
end

function count_quadrants(robots, size_x, size_y)
    counts = zeros(Int, 4)
    center_x = size_x ÷ 2
    center_y = size_y ÷ 2
    for (x, y, _, _) in robots
        if x < center_x
            if y < center_y
                counts[1] += 1
            elseif y > center_y
                counts[2] += 1
            end
        elseif x > center_x
            if y < center_y
                counts[3] += 1
            elseif y > center_y
                counts[4] += 1
            end
        end
    end
    return counts
end

function has_no_overlaps(robots)
    positions = Set{Tuple{Int, Int}}()
    for (x, y, _, _) in robots
        pos = (x, y)
        if pos in positions
            return false
        end
        push!(positions, pos)
    end
    return true
end

function draw_grid(robots, size_x, size_y)
    grid = fill('.', size_y, size_x)
    for (x, y, _, _) in robots
        grid[y+1, x+1] = '#'
    end
    for row in eachrow(grid)
        println(join(row))
    end
end

function main()
    size_x = 101
    size_y = 103
    robots = []
    open("input.txt") do file
        for line in eachline(file)
            if isempty(line)
                continue
            end
            push!(robots, parse_line(line))
        end
    end

    robots_part1 = copy(robots)
    for _ in 1:100
        move_robots!(robots_part1, size_x, size_y)
    end
    counts = count_quadrants(robots_part1, size_x, size_y)
    safety_factor = prod(counts)
    println("Part 1 - Safety Factor after 100 seconds: $safety_factor")

    robots_part2 = copy(robots)
    seconds = 0
    while true
        if has_no_overlaps(robots_part2)
            break
        end
        move_robots!(robots_part2, size_x, size_y)
        seconds += 1
        if seconds > 1000000
            println("Exceeded maximum iterations without finding a unique position configuration.")
            exit(1)
        end
    end
    println("Part 2 - Fewest seconds to display Easter egg: $seconds")
    println("Final positions of robots:")
    draw_grid(robots_part2, size_x, size_y)
end

main()
