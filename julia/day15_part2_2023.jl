
function hash_algorithm(str)
    current_value = 0
    for char in str
        current_value += Int(char)
        current_value *= 17
        current_value %= 256
    end
    return current_value
end

function part1(input)
    steps = split(strip(input), ",")
    return sum(hash_algorithm(step) for step in steps)
end

function part2(input)
    steps = split(strip(input), ",")
    boxes = [[] for _ in 1:256]
    
    for step in steps
        if occursin("-", step)
            label = split(step, "-")[1]
            box_num = hash_algorithm(label) + 1
            filter!(lens -> first(lens) != label, boxes[box_num])
        else
            label, focal_length = split(step, "=")
            box_num = hash_algorithm(label) + 1
            focal_length = parse(Int, focal_length)
            
            existing_lens_index = findfirst(lens -> first(lens) == label, boxes[box_num])
            if isnothing(existing_lens_index)
                push!(boxes[box_num], (label, focal_length))
            else
                boxes[box_num][existing_lens_index] = (label, focal_length)
            end
        end
    end
    
    total_focusing_power = 0
    for (box_idx, box) in enumerate(boxes)
        for (slot_idx, (_, focal_length)) in enumerate(box)
            total_focusing_power += box_idx * slot_idx * focal_length
        end
    end
    
    return total_focusing_power
end

function main()
    input = read("input.txt", String)
    println("Part 1: ", part1(input))
    println("Part 2: ", part2(input))
end

main()
