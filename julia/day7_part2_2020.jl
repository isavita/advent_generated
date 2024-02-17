
function countBags(color, rules)
    count = 1
    for rule in get(rules, color, [])
        count += rule.count * countBags(rule.color, rules)
    end
    return count
end

rules = Dict{String, Array{NamedTuple{(:color, :count), Tuple{String, Int}}}}()
file = open("input.txt", "r")
for line in eachline(file)
    container, contents = split(line, " bags contain ")
    if contents == "no other bags."
        continue
    end
    for match in eachmatch(r"(\d+) (\w+ \w+) bags?[,.]", contents)
        count = parse(Int, match.captures[1])
        push!(get!(rules, container, []), (color=match.captures[2], count=count))
    end
end
close(file)

totalBags = countBags("shiny gold", rules) - 1
println(totalBags)
