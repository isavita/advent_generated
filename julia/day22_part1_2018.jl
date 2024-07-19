
function main()
    data = read("input.txt", String)
    depth, target = parse_input(data)
    cave = make_cave_system(depth, target)
    risk_level = calculate_risk_level(cave, target)
    println(risk_level)
end

function parse_input(data::String)
    lines = split(data, "\n")
    depth = parse(Int, split(lines[1])[2])
    coords = split(lines[2])[2]
    x, y = parse.(Int, split(coords, ","))
    return depth, (x, y)
end

function make_cave_system(depth::Int, target::Tuple{Int, Int})
    cave = zeros(Int, target[2] + 1, target[1] + 1)
    for y in 0:target[2]
        for x in 0:target[1]
            if (x == 0 && y == 0) || (x == target[1] && y == target[2])
                geologic_index = 0
            elseif y == 0
                geologic_index = x * 16807
            elseif x == 0
                geologic_index = y * 48271
            else
                geologic_index = cave[y + 1, x] * cave[y, x + 1]
            end
            cave[y + 1, x + 1] = (geologic_index + depth) % 20183
        end
    end
    return cave
end

function calculate_risk_level(cave::AbstractMatrix, target::Tuple{Int, Int})
    return sum(cave[1:target[2] + 1, 1:target[1] + 1] .% 3)
end

main()
