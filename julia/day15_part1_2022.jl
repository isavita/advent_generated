
struct Sensor
    pos::Tuple{Int, Int}
    beacon::Tuple{Int, Int}
    dist::Int
end

function read_input(file_path)
    open(file_path, "r") do file
        return read(file, String)
    end
end

function manhattan(p, q)
    return abs(p[1] - q[1]) + abs(p[2] - q[2])
end

function impossible(sensors, y)
    pts = Set{Int}()
    for s in sensors
        dist = s.dist - abs(s.pos[2] - y)
        if dist >= 0
            for x in s.pos[1]-dist:s.pos[1]+dist
                push!(pts, x)
            end
        end
    end
    for s in sensors
        if s.beacon[2] == y
            delete!(pts, s.beacon[1])
        end
    end
    return length(pts)
end

function main()
    input = read_input("input.txt")
    sensors = Sensor[]
    for line in split(input, "\n")
        m = match(r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)", line)
        pos = (parse(Int, m.captures[1]), parse(Int, m.captures[2]))
        beacon = (parse(Int, m.captures[3]), parse(Int, m.captures[4]))
        dist = manhattan(pos, beacon)
        push!(sensors, Sensor(pos, beacon, dist))
    end
    println(impossible(sensors, 2000000))
end

main()
