function readRules(lines)
    rules = Dict{Int,String}()
    for line in lines
        isempty(line) && break
        parts = split(line, ": ")
        rules[parse(Int, parts[1])] = replace(parts[2], "\"" => "")
    end
    return rules
end

function constructPattern(rules, index)
    if occursin("|", rules[index])
        subrules = split(rules[index], " | ")
        parts = [constructSubPattern(rules, subrule) for subrule in subrules]
        return "(" * join(parts, "|") * ")"
    end
    return constructSubPattern(rules, rules[index])
end

function constructSubPattern(rules, subrule)
    if subrule == "a" || subrule == "b"
        return subrule
    end
    subIdxs = split(subrule)
    pattern = ""
    for idx in subIdxs
        pattern *= constructPattern(rules, parse(Int, idx))
    end
    return pattern
end

function countMatches(lines, pattern)
    count = 0
    re = Regex("^$pattern\$")
    for line in lines
        if match(re, line) !== nothing
            count += 1
        end
    end
    return count
end

function main()
    lines = readlines("input.txt")
    rules = readRules(lines)
    pattern = constructPattern(rules, 0)
    count = countMatches(Iterators.drop(lines, length(rules) + 1), pattern)
    println("The number of messages that completely match rule 0 is: $count")
end

main()