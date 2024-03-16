function main()
    file = open("input.txt", "r")
    regex = r"Game (\d+): (.+)"
    cubeRegex = r"(\d+) (red|green|blue)"
    totalPower = 0

    for line in eachline(file)
        matches = match(regex, line)
        if matches !== nothing
            rounds = split(matches[2], ";")
            maxRed, maxGreen, maxBlue = 0, 0, 0

            for round in rounds
                cubes = collect(eachmatch(cubeRegex, round))
                red, green, blue = 0, 0, 0

                for cube in cubes
                    count = parse(Int, cube[1])
                    color = cube[2]
                    if color == "red"
                        red += count
                    elseif color == "green"
                        green += count
                    elseif color == "blue"
                        blue += count
                    end
                end

                if red > maxRed
                    maxRed = red
                end
                if green > maxGreen
                    maxGreen = green
                end
                if blue > maxBlue
                    maxBlue = blue
                end
            end

            power = maxRed * maxGreen * maxBlue
            totalPower += power
        end
    end

    close(file)
    println(totalPower)
end

main()