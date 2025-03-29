
import Base: read

# Define struct for rules within workflows
struct Rule
    category::Union{Char, Nothing}   # 'x', 'm', 'a', 's', or nothing for fallback
    operator::Union{Char, Nothing}   # '<', '>', or nothing for fallback
    value::Union{Int, Nothing}       # Comparison value, or nothing for fallback
    destination::String              # "A", "R", or next workflow name
end

# Define type alias for workflows and parts for clarity
const Workflows = Dict{String, Vector{Rule}}
const Part = Dict{Char, Int}
const PartRanges = Dict{Char, Tuple{Int, Int}}

# --- Parsing ---

"""
    parse_input(filename::String) -> (Workflows, Vector{Part})

Parses the input file containing workflows and part ratings.
Returns a dictionary of workflows and a vector of parts.
"""
function parse_input(filename::String)
    workflows = Workflows()
    parts = Vector{Part}()
    parsing_workflows = true

    # Use do-block syntax for safe file handling
    open(filename, "r") do file
        for line in eachline(file)
            line = strip(line)
            if isempty(line)
                parsing_workflows = false # Switch to parsing parts after blank line
                continue
            end

            if parsing_workflows
                # Parse workflow: name{rule1,rule2,...}
                m = match(r"(\w+)\{(.*)\}", line)
                if m !== nothing
                    name = m.captures[1]
                    rules_str = m.captures[2]
                    rules = Rule[]
                    for rule_part in split(rules_str, ',')
                        if contains(rule_part, ':')
                            # Conditional rule: cat<op>val:dest
                            m_rule = match(r"([xmas])([<>])(\d+):(\w+|[AR])", rule_part)
                            if m_rule !== nothing
                                cat = m_rule.captures[1][1] # Get Char
                                op = m_rule.captures[2][1]  # Get Char
                                val = parse(Int, m_rule.captures[3])
                                dest = m_rule.captures[4]
                                push!(rules, Rule(cat, op, val, dest))
                            else
                                println(stderr, "Warning: Could not parse rule part: ", rule_part)
                            end
                        else
                            # Fallback rule: dest
                            push!(rules, Rule(nothing, nothing, nothing, rule_part))
                        end
                    end
                    workflows[name] = rules
                else
                    println(stderr, "Warning: Could not parse workflow line: ", line)
                end
            else
                # Parse part: {x=val,m=val,a=val,s=val}
                m = match(r"\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}", line)
                if m !== nothing
                    part = Part(
                        'x' => parse(Int, m.captures[1]),
                        'm' => parse(Int, m.captures[2]),
                        'a' => parse(Int, m.captures[3]),
                        's' => parse(Int, m.captures[4])
                    )
                    push!(parts, part)
                else
                     println(stderr, "Warning: Could not parse part line: ", line)
                end
            end
        end
    end # File is automatically closed here
    return workflows, parts
end

# --- Part 1 ---

"""
    process_part(part::Part, workflows::Workflows) -> Bool

Processes a single part through the workflows starting at "in".
Returns `true` if the part is accepted ('A'), `false` if rejected ('R').
"""
function process_part(part::Part, workflows::Workflows)::Bool
    current_wf_name = "in"
    while true
        # Handle terminal states
        if current_wf_name == "A" return true end
        if current_wf_name == "R" return false end

        # Get rules for the current workflow
        rules = workflows[current_wf_name]

        # Apply rules in order
        destination_found = false
        for rule in rules
            if isnothing(rule.category) # Fallback rule
                current_wf_name = rule.destination
                destination_found = true
                break # Move to the next workflow specified by fallback
            else
                # Conditional rule
                part_val = part[rule.category]
                condition_met = false
                if rule.operator == '<'
                    condition_met = part_val < rule.value
                elseif rule.operator == '>'
                    condition_met = part_val > rule.value
                else
                    # Should not happen with valid input
                    error("Invalid operator encountered: $(rule.operator)")
                end

                if condition_met
                    current_wf_name = rule.destination
                    destination_found = true
                    break # Rule matched, move to the specified destination
                end
                # Condition not met, continue to the next rule in this workflow
            end
        end
         # This should technically not be needed if workflows are well-formed (always end in fallback or terminal)
         !destination_found && error("Workflow $(current_wf_name) finished without reaching a destination for part $(part)")
    end
end

"""
    solve_part1(workflows::Workflows, parts::Vector{Part}) -> Int

Calculates the sum of ratings for all accepted parts.
"""
function solve_part1(workflows::Workflows, parts::Vector{Part})::Int
    total_rating_sum = 0
    for part in parts
        if process_part(part, workflows)
            total_rating_sum += sum(values(part))
        end
    end
    return total_rating_sum
end

# --- Part 2 ---

"""
    range_size(r::Tuple{Int, Int}) -> Int

Calculates the number of integers in a range (inclusive). Returns 0 if min > max.
"""
function range_size(r::Tuple{Int, Int})::Int
    # Use Int promotion for potentially large results later, though size itself fits Int
    return max(0, r[2] - r[1] + 1)
end

"""
    count_combinations(ranges::PartRanges) -> Int

Calculates the total number of distinct combinations within the given rating ranges.
Uses BigInt to avoid overflow, as the result can be very large.
"""
function count_combinations(ranges::PartRanges)::BigInt
    total = BigInt(1)
    for cat in ['x', 'm', 'a', 's']
        sz = range_size(get(ranges, cat, (1, 0))) # Default to empty range if missing
        if sz == 0 return BigInt(0) end # If any range is empty, the total is 0
        total *= sz
    end
    return total
end

"""
    count_accepted(wf_name::String, ranges::PartRanges, workflows::Workflows) -> BigInt

Recursively counts the number of accepted combinations within the given ranges,
starting from the specified workflow name. Uses BigInt for the count.
"""
function count_accepted(wf_name::String, ranges::PartRanges, workflows::Workflows)::BigInt
    # Base Cases
    if wf_name == "R" return BigInt(0) end
    # Calculate combinations for current valid ranges if accepted
    combinations = count_combinations(ranges)
    if wf_name == "A" return combinations end
    # If the current ranges represent zero combinations, stop recursion
    if combinations == 0 return BigInt(0) end

    # Recursive Step
    total_accepted = BigInt(0)
    # Ranges remaining to be processed by subsequent rules in this workflow
    current_ranges = deepcopy(ranges)

    # Get rules for the current workflow
    rules = workflows[wf_name]

    for rule in rules
        # If no combinations remain in current_ranges, stop processing rules
        if count_combinations(current_ranges) == 0
            break
        end

        if isnothing(rule.category) # Fallback rule
            # The fallback rule applies to all remaining combinations
            total_accepted += count_accepted(rule.destination, current_ranges, workflows)
            # After fallback, no ranges are left for any subsequent (non-existent) rules
            empty!(current_ranges) # Mark as empty
            break
        else
            # Conditional rule
            cat = rule.category
            op = rule.operator
            val = rule.value
            dest = rule.destination

            min_val, max_val = current_ranges[cat]

            # Define ranges that satisfy and do not satisfy the condition
            satisfied_range = (0, -1) # Initialize as empty/invalid
            unsatisfied_range = (0, -1) # Initialize as empty/invalid

            if op == '<'
                # Satisfied: values strictly less than val
                satisfied_range = (min_val, min(max_val, val - 1))
                # Unsatisfied: values greater than or equal to val
                unsatisfied_range = (max(min_val, val), max_val)
            elseif op == '>'
                # Satisfied: values strictly greater than val
                satisfied_range = (max(min_val, val + 1), max_val)
                # Unsatisfied: values less than or equal to val
                unsatisfied_range = (min_val, min(max_val, val))
            end

            # Process the part of the range that satisfies the condition
            if range_size(satisfied_range) > 0
                satisfied_ranges = deepcopy(current_ranges)
                satisfied_ranges[cat] = satisfied_range
                total_accepted += count_accepted(dest, satisfied_ranges, workflows)
            end

            # Update the current range to the part that *doesn't* satisfy the condition
            # This updated range will be processed by the *next* rule in the loop
            if range_size(unsatisfied_range) > 0
                current_ranges[cat] = unsatisfied_range
            else
                # The entire current range satisfied the condition or was already empty.
                # No possibilities left for subsequent rules in this branch.
                empty!(current_ranges) # Mark as empty
                break # Exit the rule processing loop for this path
            end
        end
    end

    return total_accepted
end

"""
    solve_part2(workflows::Workflows) -> BigInt

Calculates the total number of distinct combinations of ratings (1-4000)
that are accepted by the workflows.
"""
function solve_part2(workflows::Workflows)::BigInt
    # Initial ranges cover all possible values (1 to 4000)
    initial_ranges = PartRanges(
        'x' => (1, 4000),
        'm' => (1, 4000),
        'a' => (1, 4000),
        's' => (1, 4000)
    )
    return count_accepted("in", initial_ranges, workflows)
end

# --- Main Execution ---

"""
    main()

Main function to orchestrate the program execution.
Reads input, solves both parts, and prints the results.
"""
function main()
    input_file = "input.txt"
    if !isfile(input_file)
        println(stderr, "Error: Input file '$input_file' not found.")
        return 1 # Indicate error
    end

    # Parse the input file
    workflows, parts = parse_input(input_file)

    # Solve Part 1
    part1_result = solve_part1(workflows, parts)
    println("Part 1: ", part1_result)

    # Solve Part 2
    part2_result = solve_part2(workflows)
    println("Part 2: ", part2_result)

    return 0 # Indicate success
end

# Ensure main() is called only when the script is executed directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
