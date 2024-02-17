using DelimitedFiles

mutable struct Rule
    name::String
    ranges::Vector{Tuple{Int, Int}}
end

function is_valid(r::Rule, value::Int)
    for rng in r.ranges
        if value >= rng[1] && value <= rng[2]
            return true
        end
    end
    return false
end

function main()
    rules = Rule[]
    scanning_rules = true
    error_rate = 0

    re_rule = r"^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$"

    for line in eachline("input.txt")
        if line == ""
            continue
        end
        if startswith(line, "your ticket:") || startswith(line, "nearby tickets:")
            scanning_rules = false
            continue
        end
        if scanning_rules
            matches = match(re_rule, line)
            if matches !== nothing
                name = matches.captures[1]
                range1 = (parse(Int, matches.captures[2]), parse(Int, matches.captures[3]))
                range2 = (parse(Int, matches.captures[4]), parse(Int, matches.captures[5]))
                push!(rules, Rule(name, [range1, range2]))
            end
        else
            for value in split(line, ",")
                val = parse(Int, value)
                if !any(rule -> is_valid(rule, val), rules)
                    error_rate += val
                end
            end
        end
    end

    println(error_rate)
end

main()