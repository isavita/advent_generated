
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
    furthest = 0
    for (i, s1) in enumerate(settled)
        for (j, s2) in enumerate(settled)
            i == j && continue
            manhattan_dist = abs(s1.x - s2.x) + abs(s1.y - s2.y) + abs(s1.z - s2.z)
            furthest = max(furthest, manhattan_dist)
        end
    end
    return furthest
end

mutable struct Scanner
    number::Int
    x::Int
    y::Int
    z::Int
    relative_coords::Vector{Vector{Int}}
    rotations::Vector{Vector{Vector{Int}}}
    absolute_coords::Vector{Vector{Int}}
    absolute_coords_map::Dict{Vector{Int}, Bool}
end

function fill_absolute_coords_map!(s::Scanner)
    s.absolute_coords_map = Dict{Vector{Int}, Bool}()
    if isempty(s.absolute_coords)
        error("absolute coords not set for scanner $(s.number)")
    end
    for ac in s.absolute_coords
        s.absolute_coords_map[ac] = true
    end
end

function fill_rotations!(s::Scanner)
    pos_x = s.relative_coords
    dir2 = Vector{Vector{Int}}()
    dir3 = Vector{Vector{Int}}()
    dir4 = Vector{Vector{Int}}()
    dir5 = Vector{Vector{Int}}()
    dir6 = Vector{Vector{Int}}()
    for c in pos_x
        x, y, z = c
        push!(dir2, [x, -y, -z])
        push!(dir3, [x, -z, y])
        push!(dir4, [-y, -z, x])
        push!(dir5, [-x, -z, -y])
        push!(dir6, [y, -z, -x])
    end
    six_rotations = [pos_x, dir2, dir3, dir4, dir5, dir6]
    final_rotations = Vector{Vector{Vector{Int}}}()
    for rotation in six_rotations
        r2 = Vector{Vector{Int}}()
        r3 = Vector{Vector{Int}}()
        r4 = Vector{Vector{Int}}()
        for c in rotation
            x, y, z = c
            push!(r2, [-y, x, z])
            push!(r3, [-x, -y, z])
            push!(r4, [y, -x, z])
        end
        push!(final_rotations, rotation, r2, r3, r4)
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

function make_absolute_coords_list(absolute::Vector{Int}, relative::Vector{Int}, relative_coords::Vector{Vector{Int}})
    diff = absolute .- relative
    abs_coords = Vector{Vector{Int}}()
    for c in relative_coords
        push!(abs_coords, diff .+ c)
    end
    return abs_coords
end

function parse_input(input::String)
    scanners = Vector{Scanner}()
    for raw_scanner in split(input, "\n\n")
        lines = split(raw_scanner, "\n")
        number = parse(Int, match(r"--- scanner (\d+) ---", lines[1])[1])
        coords = Vector{Vector{Int}}()
        for line in lines[2:end]
            x, y, z = parse.(Int, split(line, ","))
            push!(coords, [x, y, z])
        end
        sc = Scanner(number, 0, 0, 0, coords, Vector{Vector{Vector{Int}}}(), Vector{Vector{Int}}(), Dict{Vector{Int}, Bool}())
        fill_rotations!(sc)
        push!(scanners, sc)
    end
    return scanners
end

println(solve())
