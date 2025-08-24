function main()
    target = parse(Int, strip(read("input.txt", String)))

    grid = Dict{Tuple{Int,Int},Int}()
    grid[(0, 0)] = 1

    x, y = 0, 0
    dx, dy = 0, -1

    while true
        if x == y || (x < 0 && x == -y) || (x > 0 && x == 1-y)
            dx, dy = -dy, dx
        end

        x += dx
        y += dy

        value = 0
        for dx in -1:1, dy in -1:1
            value += get(grid, (x+dx, y+dy), 0)
        end
        grid[(x, y)] = value

        if value > target
            println(value)
            break
        end
    end
end

main()