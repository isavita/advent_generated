
import Base: parse

# Define the structure for a Monkey
mutable struct Monkey
    id::Int
    items::Vector{Int}
    operation::Function
    test_divisor::Int
    if_true_target::Int
    if_false_target::Int
    inspection_count::Int
end

# --- Parsing Logic ---

function parse_operation(op_str::AbstractString)::Function
    parts = split(op_str, ' ')
    operator_str = parts[end-1]
    operand_str = parts[end]

    if operand_str == "old"
        if operator_str == "*"
            return old -> old * old
        elseif operator_str == "+"
            return old -> old + old
        else
            error("Unknown operator in self operation: $operator_str")
        end
    else
        operand = parse(Int, operand_str)
        if operator_str == "*"
            return old -> old * operand
        elseif operator_str == "+"
            return old -> old + operand
        else
            error("Unknown operator: $operator_str")
        end
    end
end

function parse_monkey(monkey_block::AbstractString)::Monkey
    lines = split(strip(monkey_block), '\n')

    # Monkey ID
    id = parse(Int, match(r"Monkey (\d+):", lines[1]).captures[1])

    # Starting Items
    items_str = strip(split(lines[2], ':')[2])
    items = [parse(Int, s) for s in split(items_str, ", ")]

    # Operation
    op_str = strip(split(lines[3], ':')[2]) # Should be like "new = old * 19"
    operation = parse_operation(op_str)

    # Test Divisor
    test_divisor = parse(Int, split(lines[4])[end])

    # Target Monkeys
    if_true_target = parse(Int, split(lines[5])[end])
    if_false_target = parse(Int, split(lines[6])[end])

    return Monkey(id, items, operation, test_divisor, if_true_target, if_false_target, 0)
end

function parse_input(filename::String)::Vector{Monkey}
    full_text = read(filename, String)
    monkey_blocks = split(full_text, "\n\n")
    monkeys = [parse_monkey(block) for block in monkey_blocks if !isempty(strip(block))]
    # Ensure monkeys are sorted by ID implicitly by parsing order
    sort!(monkeys, by = m -> m.id)
    return monkeys
end

# --- Simulation Logic ---

function simulate_round!(monkeys::Vector{Monkey})
    num_monkeys = length(monkeys)
    for i = 1:num_monkeys # Iterate using index because we modify the monkeys array
        monkey = monkeys[i]
        # Process items one by one, removing them from the current monkey
        while !isempty(monkey.items)
            monkey.inspection_count += 1
            old_worry = popfirst!(monkey.items) # Item is removed here

            # Apply operation
            new_worry = monkey.operation(old_worry)

            # Monkey gets bored
            bored_worry = new_worry ÷ 3 # Integer division

            # Test and throw
            if bored_worry % monkey.test_divisor == 0
                target_monkey_id = monkey.if_true_target
            else
                target_monkey_id = monkey.if_false_target
            end

            # Throw item to the target monkey (add to the end)
            # Target IDs are 0-based, Julia array indices are 1-based
            push!(monkeys[target_monkey_id + 1].items, bored_worry)
        end
    end
end

# --- Main Execution ---

function main()
    if length(ARGS) != 1
        println("Usage: julia ", basename(PROGRAM_FILE), " <input_file>")
        exit(1)
    end
    input_filename = ARGS[1]

    if !isfile(input_filename)
        println("Error: File not found '", input_filename, "'")
        exit(1)
    end

    monkeys = parse_input(input_filename)

    num_rounds = 20
    for _ = 1:num_rounds
        simulate_round!(monkeys)
    end

    # Calculate monkey business
    inspection_counts = [m.inspection_count for m in monkeys]
    sort!(inspection_counts, rev=true) # Sort descending

    if length(inspection_counts) >= 2
        monkey_business = inspection_counts[1] * inspection_counts[2]
        println(monkey_business)
    elseif length(inspection_counts) == 1
         println("Only one monkey found. Monkey business: ", inspection_counts[1])
         # Or handle as appropriate for the puzzle if this case matters
    else
        println("No monkeys found.")
    end

end

# Check if the script is run directly
if abspath(PROGRAM_FILE) == @__FILE__
    # If input.txt exists in the current directory, use it.
    # Otherwise, expect the filename as a command-line argument.
    default_input = "input.txt"
    if isfile(default_input) && isempty(ARGS)
        # Modify ARGS for the main function if using default
        push!(ARGS, default_input)
    end
    main()
end
