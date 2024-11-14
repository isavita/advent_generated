
function parse_input(filename)
    monkeys = []
    open(filename, "r") do file
        while !eof(file)
            readline(file)  # Monkey header
            items_line = readline(file)
            op_line = readline(file)
            test_line = readline(file)
            true_line = readline(file)
            false_line = readline(file)
            readline(file)  # blank line or EOF

            # Parse starting items
            items = [parse(Int, x) for x in split(split(items_line, ": ")[2], ", ")]
            
            # Parse operation
            op_parts = split(split(op_line, "= ")[2])
            op_func = if op_parts[3] == "old"
                x -> x * x
            elseif op_parts[2] == "*"
                x -> x * parse(Int, op_parts[3])
            else
                x -> x + parse(Int, op_parts[3])
            end

            # Parse test conditions
            div_test = parse(Int, split(test_line)[end])
            true_monkey = parse(Int, split(true_line)[end]) + 1
            false_monkey = parse(Int, split(false_line)[end]) + 1

            push!(monkeys, Dict(
                "items" => items,
                "op" => op_func,
                "div_test" => div_test,
                "true_monkey" => true_monkey,
                "false_monkey" => false_monkey,
                "inspections" => 0
            ))
        end
    end
    return monkeys
end

function solve_part1(filename)
    monkeys = parse_input(filename)
    
    for _ in 1:20
        for monkey in monkeys
            while !isempty(monkey["items"])
                item = popfirst!(monkey["items"])
                monkey["inspections"] += 1
                
                # Inspect and reduce worry
                worry = floor(Int, monkey["op"](item) / 3)
                
                # Test and throw
                if worry % monkey["div_test"] == 0
                    push!(monkeys[monkey["true_monkey"]]["items"], worry)
                else
                    push!(monkeys[monkey["false_monkey"]]["items"], worry)
                end
            end
        end
    end
    
    # Calculate monkey business
    inspections = [m["inspections"] for m in monkeys]
    sort!(inspections, rev=true)
    return inspections[1] * inspections[2]
end

function solve_part2(filename)
    monkeys = parse_input(filename)
    
    # Calculate product of all divisibility tests
    mod_factor = prod(m["div_test"] for m in monkeys)
    
    for _ in 1:10000
        for monkey in monkeys
            while !isempty(monkey["items"])
                item = popfirst!(monkey["items"])
                monkey["inspections"] += 1
                
                # Inspect 
                worry = monkey["op"](item)
                
                # Keep worry manageable
                worry %= mod_factor
                
                # Test and throw
                if worry % monkey["div_test"] == 0
                    push!(monkeys[monkey["true_monkey"]]["items"], worry)
                else
                    push!(monkeys[monkey["false_monkey"]]["items"], worry)
                end
            end
        end
    end
    
    # Calculate monkey business
    inspections = [m["inspections"] for m in monkeys]
    sort!(inspections, rev=true)
    return inspections[1] * inspections[2]
end

# Read from input.txt and print result
function main()
    println("Part 1: ", solve_part1("input.txt"))
    println("Part 2: ", solve_part2("input.txt"))
end

main()
