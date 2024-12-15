
function solve()
    rules, updates = read_input("input.txt")
    sum = 0
    for update in updates
        if !is_correctly_ordered(update, rules)
            sorted_update = sort_update(update, rules)
            sum += sorted_update[div(length(sorted_update), 2) + 1]
        end
    end
    println(sum)
end

function read_input(filename)
    rules = Vector{Vector{Int}}()
    updates = Vector{Vector{Int}}()
    is_update_section = false
    open(filename, "r") do file
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
                x = parse(Int, strip(parts[1]))
                y = parse(Int, strip(parts[2]))
                push!(rules, [x, y])
            else
                nums = split(line, ",")
                update = Int[]
                for num_str in nums
                    num = parse(Int, strip(num_str))
                    push!(update, num)
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
    position = Dict(page => idx for (idx, page) in enumerate(update))
    for rule in rules
        x, y = rule
        if haskey(position, x) && haskey(position, y)
            if position[x] >= position[y]
                return false
            end
        end
    end
    return true
end

function sort_update(update, rules)
    adjacency = Dict{Int, Vector{Int}}()
    pages_in_update = Set(update)
    for page in update
        adjacency[page] = Int[]
    end
    for rule in rules
        x, y = rule
        if x in pages_in_update && y in pages_in_update
            push!(adjacency[x], y)
        end
    end
    visited = Set{Int}()
    temp_marked = Set{Int}()
    result = Int[]
    function visit(n)
        if n in temp_marked
            error("Cycle detected")
        end
        if !(n in visited)
            push!(temp_marked, n)
            for m in get(adjacency, n, [])
                visit(m)
            end
            delete!(temp_marked, n)
            push!(visited, n)
            push!(result, n)
        end
    end
    for page in pages_in_update
        if !(page in visited)
            visit(page)
        end
    end
    reverse!(result)
    return result
end

solve()
