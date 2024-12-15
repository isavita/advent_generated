
function solve()
    input = read("input.txt", String)
    scanners = parse_input(input)
    settled = [scanners[1]]
    settled[1].absolute_coords = settled[1].relative_coords
    fill_absolute_coords_map!(settled[1])
    undetermined = scanners[2:end]
    while !isempty(undetermined)
        for (i, undet) in enumerate(undetermined)
            maybe_updated, ok = find_absolute_coords_for_scanner(undet, settled)
            if ok
                push!(settled, maybe_updated)
                deleteat!(undetermined, i)
                break
            end
        end
    end
    all_beacons = Set{Tuple{Int, Int, Int}}()
    for s in settled
        for c in keys(s.absolute_coords_map)
            push!(all_beacons, c)
        end
    end
    return length(all_beacons)
end

mutable struct Scanner
    number::Int
    x::Int
    y::Int
    z::Int
    relative_coords::Vector{Tuple{Int, Int, Int}}
    rotations::Vector{Vector{Tuple{Int, Int, Int}}}
    absolute_coords::Vector{Tuple{Int, Int, Int}}
    absolute_coords_map::Dict{Tuple{Int, Int, Int}, Bool}
end

function fill_absolute_coords_map!(s::Scanner)
    s.absolute_coords_map = Dict{Tuple{Int, Int, Int}, Bool}()
    if isempty(s.absolute_coords)
        error("absolute coords not set for scanner $(s.number)")
    end
    for ac in s.absolute_coords
        s.absolute_coords_map[ac] = true
    end
end

function fill_rotations!(s::Scanner)
    pos_x = s.relative_coords
    dir2 = map(c -> (c[1], -c[2], -c[3]), pos_x)
    dir3 = map(c -> (c[1], -c[3], c[2]), pos_x)
    dir4 = map(c -> (-c[2], -c[3], c[1]), pos_x)
    dir5 = map(c -> (-c[1], -c[3], -c[2]), pos_x)
    dir6 = map(c -> (c[2], -c[3], -c[1]), pos_x)
    six_rotations = [pos_x, dir2, dir3, dir4, dir5, dir6]
    final_rotations = Vector{Vector{Tuple{Int, Int, Int}}}()
    for rotation in six_rotations
        r2 = map(c -> (-c[2], c[1], c[3]), rotation)
        r3 = map(c -> (-c[1], -c[2], c[3]), rotation)
        r4 = map(c -> (c[2], -c[1], c[3]), rotation)
        append!(final_rotations, [rotation, r2, r3, r4])
    end
    s.rotations = final_rotations
end

function find_absolute_coords_for_scanner(undet::Scanner, settled::Vector{Scanner})
    for rotated_coords in undet.rotations
        for set in settled
            for abs_coord in set.absolute_coords
                for relative_coord in rotated_coords
                    unsettled_absolute_coords = make_absolute_coords_list(abs_coord, relative_coord, rotated_coords)
                    matching_count = 0
                    for ac in unsettled_absolute_coords
                        if haskey(set.absolute_coords_map, ac)
                            matching_count += 1
                        end
                    end
                    if matching_count >= 12
                        undet.relative_coords = rotated_coords
                        undet.absolute_coords = unsettled_absolute_coords
                        fill_absolute_coords_map!(undet)
                        undet.x = abs_coord[1] - relative_coord[1]
                        undet.y = abs_coord[2] - relative_coord[2]
                        undet.z = abs_coord[3] - relative_coord[3]
                        return undet, true
                    end
                end
            end
        end
    end
    return undet, false
end

function make_absolute_coords_list(absolute::Tuple{Int, Int, Int}, relative::Tuple{Int, Int, Int}, relative_coords::Vector{Tuple{Int, Int, Int}})
    diff = (absolute[1] - relative[1], absolute[2] - relative[2], absolute[3] - relative[3])
    abs_coords = map(c -> (diff[1] + c[1], diff[2] + c[2], diff[3] + c[3]), relative_coords)
    return abs_coords
end

function parse_input(input::String)
    scanners = Scanner[]
    for raw_scanner in split(input, "\n\n")
        lines = split(raw_scanner, "\n")
        number = parse(Int, match(r"--- scanner (\d+) ---", lines[1])[1])
        coords = map(lines[2:end]) do line
            x, y, z = map(x -> parse(Int, x), split(line, ","))
            (x, y, z)
        end
        sc = Scanner(number, 0, 0, 0, coords, Vector{Vector{Tuple{Int, Int, Int}}}(), Vector{Tuple{Int, Int, Int}}(), Dict{Tuple{Int, Int, Int}, Bool}())
        fill_rotations!(sc)
        push!(scanners, sc)
    end
    return scanners
end

println(solve())
