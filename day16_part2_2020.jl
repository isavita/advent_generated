struct Rule
    name::String
    ranges::Vector{Tuple{Int,Int}}
end

function isvalid(r::Rule, value::Int)
    for rng in r.ranges
        if value >= rng[1] && value <= rng[2]
            return true
        end
    end
    return false
end

function parseticket(s::String)
    return [parse(Int, v) for v in split(s, ",")]
end

function isvalidticket(ticket::Vector{Int}, rules::Vector{Rule})
    for value in ticket
        if !any(rule -> isvalid(rule, value), rules)
            return false
        end
    end
    return true
end

function solvefieldpositions(rules::Vector{Rule}, tickets::Vector{Vector{Int}})
    validpositions = Dict(rule.name => Set(1:length(tickets[1])) for rule in rules)
    for ticket in tickets
        for (i, value) in enumerate(ticket)
            for rule in rules
                if !isvalid(rule, value)
                    delete!(validpositions[rule.name], i)
                end
            end
        end
    end

    fieldpositions = Dict{String,Int}()
    while length(fieldpositions) < length(rules)
        for (name, positions) in validpositions
            if length(positions) == 1
                pos = pop!(positions)
                fieldpositions[name] = pos
                for (othername, otherpositions) in validpositions
                    delete!(otherpositions, pos)
                end
            end
        end
    end
    return fieldpositions
end

function calculatedepartureproduct(ticket::Vector{Int}, fieldpositions::Dict{String,Int})
    product = 1
    for (name, pos) in fieldpositions
        if startswith(name, "departure")
            product *= ticket[pos]
        end
    end
    return product
end

function main()
    rules = Rule[]
    myticket = Int[]
    nearbytickets = Vector{Int}[]
    section = 0
    rerule = r"^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$"

    open("input.txt") do file
        for line in eachline(file)
            if isempty(line)
                section += 1
                continue
            end
            if section == 0
                m = match(rerule, line)
                if m !== nothing
                    name = m.captures[1]
                    ranges = [(parse(Int, m.captures[2]), parse(Int, m.captures[3])),
                              (parse(Int, m.captures[4]), parse(Int, m.captures[5]))]
                    push!(rules, Rule(name, ranges))
                end
            elseif section == 1
                if line != "your ticket:"
                    myticket = parseticket(line)
                end
            elseif section == 2
                if line != "nearby tickets:"
                    ticket = parseticket(line)
                    if isvalidticket(ticket, rules)
                        push!(nearbytickets, ticket)
                    end
                end
            end
        end
    end

    fieldpositions = solvefieldpositions(rules, nearbytickets)
    departureproduct = calculatedepartureproduct(myticket, fieldpositions)

    println(departureproduct)
end

main()