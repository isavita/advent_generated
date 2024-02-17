
function countCanContain(target, contains, seen)
    dfs(bag) = begin
        for outer in get(contains, bag, [])
            if !(outer in keys(seen))
                seen[outer] = true
                dfs(outer)
            end
        end
    end

    dfs(target)
    return length(keys(seen))
end

contains = Dict{String, Vector{String}}()
seen = Dict{String, Bool}()

open("input.txt") do file
    for line in eachline(file)
        parts = split(line, " bags contain ")
        container = parts[1]
        if parts[2] == "no other bags."
            continue
        end
        containedBags = split(parts[2], ", ")
        for bag in containedBags
            bagName = join(split(bag)[2:3], " ")
            push!(get!(contains, bagName, []), container)
        end
    end
end

count = countCanContain("shiny gold", contains, seen)
println(count)
