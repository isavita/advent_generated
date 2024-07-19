
struct Coord
    x::Float64
    y::Float64
    z::Float64
end

struct Point
    pos::Coord
    vel::Coord
end

function parse_input(input)
    points = Vector{Point}(undef, length(input))
    for (i, line) in enumerate(input)
        p = split(line, r"[ ,@]+")
        points[i] = Point(Coord(parse(Float64, p[1]), parse(Float64, p[2]), parse(Float64, p[3])),
                          Coord(parse(Float64, p[4]), parse(Float64, p[5]), parse(Float64, p[6])))
    end
    return points
end

function is_intersecting_2d(p1::Point, p2::Point)
    det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
    if det == 0
        return false, Coord(0, 0, 0), 0, 0
    end
    t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
    t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
    coord = Coord(p1.pos.x + p1.vel.x * t1, p1.pos.y + p1.vel.y * t1, 0)
    return true, coord, t1, t2
end

function solve(input, min, max)
    points = parse_input(input)
    cnt = 0
    for i in 1:length(points)
        for j in 1:i-1
            isIntersecting, coord, time1, time2 = is_intersecting_2d(points[i], points[j])
            isInBound = min <= coord.x <= max && min <= coord.y <= max
            if isIntersecting && isInBound && time1 >= 0 && time2 >= 0
                cnt += 1
            end
        end
    end
    return cnt
end

function read_file(file_name)
    return readlines(file_name)
end

input = read_file("input.txt")
println(solve(input, 200000000000000, 400000000000000))
