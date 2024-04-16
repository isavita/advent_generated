struct Rule
    resolved::Vector{String}
    options::Vector{Vector{Int}}
end

function parse_input(input::String)
    parts = split(input, "\n\n")
    rules = Dict{Int,Rule}()
    for r in split(parts[1], "\n")
        if occursin(r"[a-z]", r)
            num, char = match(r"(\d+): \"(\w)\"", r).captures
            rules[parse(Int, num)] = Rule([string(char)], [])
        else
            key, options_str = split(r, ": ")
            key = parse(Int, key)
            options = [parse.(Int, split(option, " ")) for option in split(options_str, " | ")]
            rules[key] = Rule([], options)
        end
    end
    messages = split(parts[2], "\n")
    return rules, messages
end

function fill_in_graph!(graph::Dict{Int,Rule}, entry::Int)
    if !isempty(graph[entry].resolved)
        return copy(graph[entry].resolved)
    end
    for option in graph[entry].options
        resolved = [""]
        for entry_point in option
            nested_resolve_vals = fill_in_graph!(graph, entry_point)
            new_resolved = []
            for next_piece in nested_resolve_vals, prev in resolved
                push!(new_resolved, prev * next_piece)
            end
            resolved = new_resolved
        end
        append!(graph[entry].resolved, resolved)
    end
    return graph[entry].resolved
end

function solve(input::String)
    graph, messages = parse_input(input)
    fill_in_graph!(graph, 42)
    fill_in_graph!(graph, 31)
    part42 = join(graph[42].resolved, "|")
    part31 = join(graph[31].resolved, "|")
    rule8_string = "($part42)+"
    function make_regexp(num::Int)
        return Regex("^$rule8_string($part42){$num}($part31){$num}\$")
    end
    match_rule_zero = 0
    for m in messages
        for i in 1:9
            pattern = make_regexp(i)
            if match(pattern, m) !== nothing
                match_rule_zero += 1
                break
            end
        end
    end
    return match_rule_zero
end

input = read("input.txt", String)
result = solve(input)
println(result)