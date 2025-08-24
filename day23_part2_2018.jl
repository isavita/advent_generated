struct Coordinate
    X::Int
    Y::Int
    Z::Int
end

const Zero = Coordinate(0, 0, 0)

distance(c::Coordinate, a::Coordinate) = abs(c.X - a.X) + abs(c.Y - a.Y) + abs(c.Z - a.Z)

struct Bots
    data::Dict{Coordinate,Vector{Int}}
end

function parse_input(data::String)
    regex = r"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)"
    m = match(regex, data)
    if m !== nothing
        x, y, z, r = parse.(Int, m.captures)
        return Coordinate(x, y, z), r
    else
        error("Invalid input format: $data")
    end
end

function NewBots(input)
    m = Dict{Coordinate,Vector{Int}}()
    for data in input
        c, r = parse_input(data)
        m[c] = push!(get(m, c, Int[]), r)
    end
    return Bots(m)
end

function have_in_range(m::Bots, pos::Coordinate)
    sum = 0
    for (c, rs) in m.data
        for r in rs
            if distance(pos, c) <= r
                sum += 1
            end
        end
    end
    return sum
end

function strongest_reachable(bots::Bots)
    largest_radius = 0
    largest_pos = Zero
    for (c, rs) in bots.data
        for r in rs
            if r > largest_radius
                largest_pos = c
                largest_radius = r
            end
        end
    end
    count = 0
    for (c, rs) in bots.data
        if distance(largest_pos, c) <= largest_radius
            count += length(rs)
        end
    end
    return count
end

function closest_success(bots::Bots)
    cur = Zero
    top_left = Zero
    bottom_right = Zero
    zoom = 1 << (sizeof(Int) * 8 - 2)
    while true
        zoomed_bots = Bots(Dict{Coordinate,Vector{Int}}())
        best = (pos = Zero, count = 0)
        for (c, rs) in bots.data
            for r in rs
                zc = Coordinate(c.X ÷ zoom, c.Y ÷ zoom, c.Z ÷ zoom)
                zoomed_bots.data[zc] = push!(get(zoomed_bots.data, zc, Int[]), r ÷ zoom)
            end
        end
        for cur_x in top_left.X:bottom_right.X
            for cur_y in top_left.Y:bottom_right.Y
                for cur_z in top_left.Z:bottom_right.Z
                    cur = Coordinate(cur_x, cur_y, cur_z)
                    c = have_in_range(zoomed_bots, cur)
                    if c < best.count
                        continue
                    end
                    if c == best.count && distance(Zero, cur) >= distance(Zero, best.pos)
                        continue
                    end
                    best = (pos = cur, count = c)
                end
            end
        end
        top_left = Coordinate((best.pos.X - 1) << 1, (best.pos.Y - 1) << 1, (best.pos.Z - 1) << 1)
        bottom_right = Coordinate((best.pos.X + 1) << 1, (best.pos.Y + 1) << 1, (best.pos.Z + 1) << 1)
        zoom >>= 1
        if zoom == 0
            return distance(Zero, best.pos)
        end
    end
end

input = readlines("input.txt")
bots = NewBots(input)
println(closest_success(bots))