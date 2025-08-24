function apply_insertion(polymer, rules)
    new_polymer = []
    for i in 1:length(polymer)-1
        push!(new_polymer, polymer[i])
        if haskey(rules, polymer[i:i+1])
            push!(new_polymer, rules[polymer[i:i+1]])
        end
    end
    push!(new_polymer, polymer[end])
    return join(new_polymer)
end

function count_elements(polymer)
    counts = Dict{Char,Int}()
    for c in polymer
        counts[c] = get(counts, c, 0) + 1
    end
    return counts
end

function min_max(counts)
    min_val, max_val = typemax(Int), 0
    for count in values(counts)
        min_val = min(min_val, count)
        max_val = max(max_val, count)
    end
    return min_val, max_val
end

open("input.txt") do file
    polymer = readline(file)
    rules = Dict{String,Char}()

    for line in eachline(file)
        isempty(line) && continue
        parts = split(line, " -> ")
        rules[parts[1]] = only(parts[2])
    end

    for step in 1:10
        polymer = apply_insertion(polymer, rules)
    end

    counts = count_elements(polymer)
    min_val, max_val = min_max(counts)

    println(max_val - min_val)
end