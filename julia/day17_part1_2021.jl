
function solve()
    input = read("input.txt", String)
    m = match(r"x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)", input)
    xmin, xmax, ymin, ymax = parse.(Int, m.captures)

    max_height = typemin(Int)
    for xvel in -1000:1000
        for yvel in -1000:1000
            x, y = 0, 0
            cur_xvel, cur_yvel = xvel, yvel
            highest_y = y

            while true
                x += cur_xvel
                y += cur_yvel

                highest_y = max(highest_y, y)

                if xmin <= x <= xmax && ymin <= y <= ymax
                    max_height = max(max_height, highest_y)
                    break
                end

                if (x < xmin && cur_xvel < 0) || 
                   (x > xmax && cur_xvel > 0) || 
                   (y < ymin && cur_yvel < 0)
                    break
                end

                cur_xvel = cur_xvel > 0 ? cur_xvel - 1 : 
                           cur_xvel < 0 ? cur_xvel + 1 : 0
                cur_yvel -= 1
            end
        end
    end

    return max_height
end

println(solve())
