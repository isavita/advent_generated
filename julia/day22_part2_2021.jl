function part1_out_of_bounds(x1, x2, y1, y2, z1, z2)
    if x1 < -50 || x2 > 50 || y1 < -50 || y2 > 50 || z1 < -50 || z2 > 50
        return true
    end
    return false
end

function get_intersection(c1, c2)
    x1 = max(c1.x1, c2.x1)
    x2 = min(c1.x2, c2.x2)
    y1 = max(c1.y1, c2.y1)
    y2 = min(c1.y2, c2.y2)
    z1 = max(c1.z1, c2.z1)
    z2 = min(c1.z2, c2.z2)

    if x1 > x2 || y1 > y2 || z1 > z2
        return (;isOn=false, x1=0, x2=0, y1=0, y2=0, z1=0, z2=0), false
    end

    intersection_state = c1.isOn && c2.isOn ? false : !c1.isOn && !c2.isOn ? true : c2.isOn

    return (;isOn=intersection_state, x1=x1, x2=x2, y1=y1, y2=y2, z1=z1, z2=z2), true
end

function volume(c)
    vol = (c.x2 - c.x1 + 1) * (c.y2 - c.y1 + 1) * (c.z2 - c.z1 + 1)
    return c.isOn ? vol : -vol
end

function parse_input(input)
    cubes = []
    for line in split(input, "\n")
        parts = split(line, " ")
        coords = split(parts[2], ",")
        x1, x2 = parse.(Int, split(coords[1], "=")[2] |> x -> split(x, ".."))
        y1, y2 = parse.(Int, split(coords[2], "=")[2] |> x -> split(x, ".."))
        z1, z2 = parse.(Int, split(coords[3], "=")[2] |> x -> split(x, ".."))
        push!(cubes, (;isOn=parts[1] == "on", x1=x1, x2=x2, y1=y1, y2=y2, z1=z1, z2=z2))
    end
    return cubes
end

function part1(input)
    cubes = parse_input(input)
    on_coords = Dict{Tuple{Int,Int,Int},Bool}()

    for c in cubes
        if part1_out_of_bounds(c.x1, c.x2, c.y1, c.y2, c.z1, c.z2)
            continue
        end

        for x in c.x1:c.x2, y in c.y1:c.y2, z in c.z1:c.z2
            on_coords[(x, y, z)] = c.isOn
        end
    end

    return sum(values(on_coords))
end

function solve(input)
    cubes = parse_input(input)
    final_list = []

    for c in cubes
        to_add = []
        for final_cube in final_list
            intersection, did_intersect = get_intersection(final_cube, c)
            if did_intersect
                push!(to_add, intersection)
            end
        end

        if c.isOn
            push!(to_add, c)
        end

        append!(final_list, to_add)
    end

    total = 0
    for c in final_list
        total += volume(c)
    end

    return total
end

input = read("input.txt", String)
println(solve(input))