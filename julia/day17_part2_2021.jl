using Parsers

function main()
    open("input.txt", "r") do file
        line = readline(file)
        parts = split(line, ", ")
        xrange = parse.(Int, split(parts[1][16:end], ".."))
        yrange = parse.(Int, split(parts[2][3:end], ".."))
        xmin, xmax = xrange
        ymin, ymax = yrange

        velocities = Dict{String,Bool}()
        for xvel in -1000:1000
            for yvel in -1000:1000
                xpos, ypos = 0, 0
                curxvel, curyvel = xvel, yvel
                in_target_area = false
                while true
                    xpos += curxvel
                    ypos += curyvel

                    if xmin <= xpos <= xmax && ymin <= ypos <= ymax
                        in_target_area = true
                        break
                    end

                    if is_moving_away(xpos, ypos, curxvel, curyvel, xmin, xmax, ymin, ymax)
                        break
                    end

                    if curxvel > 0
                        curxvel -= 1
                    elseif curxvel < 0
                        curxvel += 1
                    end

                    curyvel -= 1
                end

                if in_target_area
                    velocities["$xvel,$yvel"] = true
                end
            end
        end

        println(length(velocities))
    end
end

function is_moving_away(xpos, ypos, xvel, yvel, xmin, xmax, ymin, ymax)
    (xpos < xmin && xvel < 0) || (xpos > xmax && xvel > 0) || (ypos < ymin && yvel < 0)
end

main()