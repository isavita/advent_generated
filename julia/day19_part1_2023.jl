
using Printf

struct Condition
    var::String
    op::Function
    val::Int
end

struct Rule
    condition::Union{Condition, Nothing}
    destination::String
end

struct Part
    ratings::Dict{String, Int}
end

function parse_condition(condition_str::AbstractString)::Union{Condition, Nothing}
    if !occursin(":", condition_str)
        return nothing
    end
    condition_part, _ = split(condition_str, ':', limit=2)
    m = match(r"(\w+)([<>])(\d+)", condition_part)
    if isnothing(m)
        error("Invalid condition format: $condition_part")
    end
    var, op_str, val_str = m.captures
    val = parse(Int, val_str)
    op = op_str == "<" ? (<) : (>)
    return Condition(var, op, val)
end

function parse_rule(rule_str::AbstractString)::Rule
    if occursin(":", rule_str)
        condition_part, destination = split(rule_str, ':', limit=2)
        condition = parse_condition(rule_str) # Re-parse to get Condition struct
        return Rule(condition, destination)
    else
        return Rule(nothing, rule_str) # Default rule
    end
end

function read_input(file_path::String)
    workflows = Dict{String, Vector{Rule}}()
    parts = Vector{Part}()
    lines = readlines(file_path)

    blank_line_index = findfirst(isempty, lines)
    if isnothing(blank_line_index)
        blank_line_index = length(lines) + 1
    end

    workflow_lines = lines[1:blank_line_index-1]
    part_lines = lines[blank_line_index+1:end]

    workflow_pattern = r"(\w+)\{([^}]+)\}"
    for line in workflow_lines
        line = strip(line)
        isempty(line) && continue
        m = match(workflow_pattern, line)
        if !isnothing(m)
            workflow_name = m.captures[1]
            rules_str = m.captures[2]
            rules = Rule[]
            for rule_part in split(rules_str, ',')
                push!(rules, parse_rule(strip(rule_part)))
            end
            workflows[workflow_name] = rules
        else
             println("Skipping invalid workflow line: $line")
        end
    end

    part_pattern = r"\{([^}]+)\}"
    for line in part_lines
        line = strip(line)
        isempty(line) && continue
        m = match(part_pattern, line)
        if !isnothing(m)
            ratings_str = m.captures[1]
            ratings = Dict{String, Int}()
            for rating in split(ratings_str, ',')
                key, value_str = split(rating, '=')
                ratings[strip(key)] = parse(Int, strip(value_str))
            end
            push!(parts, Part(ratings))
        else
            println("Skipping invalid part line: $line")
        end
    end

    return workflows, parts
end

function evaluate_condition(part::Part, condition::Condition)::Bool
    if !haskey(part.ratings, condition.var)
        return false
    end
    return condition.op(part.ratings[condition.var], condition.val)
end

function process_part(workflows::Dict{String, Vector{Rule}}, part::Part)::Bool
    current_workflow = "in"
    while true
        if !haskey(workflows, current_workflow)
            error("Workflow '$current_workflow' not found.")
            # Or handle as rejection: return false
        end

        rules = workflows[current_workflow]
        next_workflow = ""
        rule_applied = false

        for rule in rules
            destination = rule.destination
            apply_rule = false

            if isnothing(rule.condition)
                apply_rule = true # Default rule always applies if reached
            else
                if evaluate_condition(part, rule.condition)
                    apply_rule = true
                end
            end

            if apply_rule
                if destination == "A"
                    return true
                elseif destination == "R"
                    return false
                else
                    next_workflow = destination
                    rule_applied = true
                    break
                end
            end
        end

        if rule_applied
             current_workflow = next_workflow
        else
             # Should not happen if workflows always end with a default rule
             println("Warning: No rule applied in workflow '$current_workflow' for part $part")
             return false # Reject if no rule matches (safety)
        end
    end
end

function main()
    input_file = "input.txt"
    workflows, parts = read_input(input_file)

    total_sum = 0

    for part in parts
        if process_part(workflows, part)
            part_sum = part.ratings["x"] + part.ratings["m"] + part.ratings["a"] + part.ratings["s"]
            total_sum += part_sum
        end
    end

    println(total_sum)
end

main()
