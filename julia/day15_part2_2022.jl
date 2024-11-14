
struct Sensor
    pos::Tuple{Int, Int}
    beacon::Tuple{Int, Int}
    dist::Int
end

function read_input(filename::String)
    open(filename, "r") do file
        return read(file, String)
    end
end

function parse_sensors(input::String)
    sensors = Sensor[]
    for line in split(input, "\n")
        m = match(r"x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)", line)
        pos = (parse(Int, m.captures[1]), parse(Int, m.captures[2]))
        beacon = (parse(Int, m.captures[3]), parse(Int, m.captures[4]))
        dist = abs(pos[1] - beacon[1]) + abs(pos[2] - beacon[2])
        push!(sensors, Sensor(pos, beacon, dist))
    end
    return sensors
end

function distress(sensors::Vector{Sensor}, maxcoord::Int)
    for x in 0:maxcoord
        y = 0
        while y <= maxcoord
            p = (x, y)
            detected = false
            skip = 0
            for s in sensors
                if abs(s.pos[1] - p[1]) + abs(s.pos[2] - p[2]) <= s.dist
                    detected = true
                    dist = s.dist - abs(s.pos[1] - p[1])
                    skip = max(skip, dist + s.pos[2] - p[2])
                end
            end
            if !detected
                return x * 4000000 + y
            end
            y += skip + 1
        end
    end
    return -1
end

input = read_input("input.txt")
sensors = parse_sensors(input)
println(distress(sensors, 4000000))
