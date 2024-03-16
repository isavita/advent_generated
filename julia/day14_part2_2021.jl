using Dates

function readinput(filename)
    template = ""
    rules = Dict{String,String}()
    open(filename, "r") do file
        template = readline(file)
        readline(file)
        while !eof(file)
            line = readline(file)
            parts = split(line, " -> ")
            rules[parts[1]] = parts[2]
        end
    end
    return template, rules
end

function soln()
    template, rules = readinput("input.txt")
    pairCounts = Dict{String,Int64}()
    for i in 1:length(template)-1
        pairCounts[template[i:i+1]] = get(pairCounts, template[i:i+1], 0) + 1
    end

    for step in 1:40
        newPairCounts = Dict{String,Int64}()
        for (pair, count) in pairCounts
            if haskey(rules, pair)
                newPairCounts[pair[1:1] * rules[pair]] = get(newPairCounts, pair[1:1] * rules[pair], 0) + count
                newPairCounts[rules[pair] * pair[2:2]] = get(newPairCounts, rules[pair] * pair[2:2], 0) + count
            else
                newPairCounts[pair] = get(newPairCounts, pair, 0) + count
            end
        end
        pairCounts = newPairCounts
    end

    elementCounts = Dict{Char,Int64}()
    for (pair, count) in pairCounts
        elementCounts[pair[1]] = get(elementCounts, pair[1], 0) + count
    end
    elementCounts[template[end]] += 1

    maxCount, minCount = maximum(values(elementCounts)), minimum(values(elementCounts))
    println(maxCount - minCount)
end

soln()