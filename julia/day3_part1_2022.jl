function item_priority(item::Char)
    if 'a' <= item <= 'z'
        return Int(item - 'a') + 1
    else
        return Int(item - 'A') + 27
    end
end

function main()
    file = open("input.txt", "r")
    sum = 0
    for line in eachline(file)
        half = length(line) ÷ 2
        first_compartment = line[1:half]
        second_compartment = line[half+1:end]
        
        compartment_map = Dict{Char,Int}()
        for item in first_compartment
            compartment_map[item] = get(compartment_map, item, 0) + 1
        end
        for item in second_compartment
            if haskey(compartment_map, item)
                sum += item_priority(item)
                break
            end
        end
    end
    close(file)
    println(sum)
end

main()