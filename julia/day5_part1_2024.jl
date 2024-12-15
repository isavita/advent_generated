
function solve()
    rules, updates = read_input("input.txt")
    sum = 0
    for update in updates
        if is_correctly_ordered(update, rules)
            sum += update[div(length(update), 2) + 1]
        end
    end
    println(sum)
end

function read_input(filename)
    rules = Vector{Vector{Int}}()
    updates = Vector{Vector{Int}}()
    is_update_section = false
    open(filename) do file
        for line in eachline(file)
            line = strip(line)
            if isempty(line)
                is_update_section = true
                continue
            end
            if !is_update_section
                parts = split(line, "|")
                if length(parts) != 2
                    continue
                end
                try
                    x = parse(Int, strip(parts[1]))
                    y = parse(Int, strip(parts[2]))
                    push!(rules, [x, y])
                catch
                    continue
                end
            else
                nums = split(line, ",")
                update = Int[]
                for num_str in nums
                    try
                        num = parse(Int, strip(num_str))
                        push!(update, num)
                    catch
                        continue
                    end
                end
                if !isempty(update)
                    push!(updates, update)
                end
            end
        end
    end
    return rules, updates
end

function is_correctly_ordered(update, rules)
    position = Dict{Int, Int}()
    for (idx, page) in enumerate(update)
        position[page] = idx
    end
    for rule in rules
        x, y = rule[1], rule[2]
        posX = get(position, x, 0)
        posY = get(position, y, 0)
        if posX != 0 && posY != 0
            if posX >= posY
                return false
            end
        end
    end
    return true
end

solve()
