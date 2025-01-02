
function read_asteroids(filename)
    asteroids = []
    open(filename, "r") do file
        for line in eachline(file)
            push!(asteroids, [c == '#' for c in line])
        end
    end
    asteroids
end

function vaporize_asteroids(asteroids, station)
    targets = []
    for (y, row) in enumerate(asteroids)
        for (x, is_asteroid) in enumerate(row)
            if is_asteroid && (x - 1 != station[1] || y - 1 != station[2])
                angle = atan(y - 1 - station[2], x - 1 - station[1])
                dist = sqrt((x - 1 - station[1])^2 + (y - 1 - station[2])^2)
                if angle < -pi / 2
                    angle += 2 * pi
                end
                push!(targets, (x - 1, y - 1, angle, dist))
            end
        end
    end

    sort!(targets, by=t -> (t[3], t[4]))

    vaporized = []
    while !isempty(targets)
        last_angle = -Inf
        i = 1
        while i <= length(targets)
            if targets[i][3] != last_angle
                push!(vaporized, targets[i])
                last_angle = targets[i][3]
                deleteat!(targets, i)
            else
                i += 1
            end
        end
    end
    vaporized
end

function find_best_asteroid_location(asteroids)
    best_location = (0, 0)
    max_count = 0
    for (y, row) in enumerate(asteroids)
        for (x, is_asteroid) in enumerate(row)
            if is_asteroid
                count = count_visible_asteroids(asteroids, x - 1, y - 1)
                if count > max_count
                    max_count = count
                    best_location = (x - 1, y - 1)
                end
            end
        end
    end
    best_location, max_count
end

function count_visible_asteroids(asteroids, x, y)
    angles = Set()
    for (other_y, row) in enumerate(asteroids)
        for (other_x, is_asteroid) in enumerate(row)
            if is_asteroid && (other_x - 1 != x || other_y - 1 != y)
                angle = atan(other_y - 1 - y, other_x - 1 - x)
                push!(angles, angle)
            end
        end
    end
    length(angles)
end

asteroids = read_asteroids("input.txt")
station, _ = find_best_asteroid_location(asteroids)
vaporized = vaporize_asteroids(asteroids, station)
if length(vaporized) >= 200
    result = vaporized[200][1] * 100 + vaporized[200][2]
    println(result)
else
    println("Less than 200 asteroids were vaporized.")
end
