
function parse_monkeys(filename)
    monkeys = Dict{String, Any}()
    open(filename, "r") do file
        for line in eachline(file)
            parts = split(line, ": ")
            monkey = parts[1]
            job = parts[2]
            
            if tryparse(Int, job) !== nothing
                monkeys[monkey] = parse(Int, job)
            else
                monkeys[monkey] = split(job, " ")
            end
        end
    end
    return monkeys
end

function calculate_monkey(monkeys, monkey)
    job = monkeys[monkey]
    
    if isa(job, Int)
        return job
    end
    
    left, op, right = job
    left_val = calculate_monkey(monkeys, left)
    right_val = calculate_monkey(monkeys, right)
    
    if op == "+"
        return left_val + right_val
    elseif op == "-"
        return left_val - right_val
    elseif op == "*"
        return left_val * right_val
    elseif op == "/"
        return left_val ÷ right_val
    end
end

function has_human(monkeys, monkey)
    job = monkeys[monkey]
    
    if monkey == "humn"
        return true
    end
    
    if isa(job, Int)
        return false
    end
    
    left, _, right = job
    return has_human(monkeys, left) || has_human(monkeys, right)
end

function find_human_value(monkeys, monkey, target)
    if monkey == "humn"
        return target
    end
    
    job = monkeys[monkey]
    left, op, right = job
    
    if has_human(monkeys, left)
        other_val = calculate_monkey(monkeys, right)
        
        if op == "+"
            new_target = target - other_val
        elseif op == "-"
            new_target = target + other_val
        elseif op == "*"
            new_target = target ÷ other_val
        elseif op == "/"
            new_target = target * other_val
        end
        
        return find_human_value(monkeys, left, new_target)
    else
        other_val = calculate_monkey(monkeys, left)
        
        if op == "+"
            new_target = target - other_val
        elseif op == "-"
            new_target = other_val - target
        elseif op == "*"
            new_target = target ÷ other_val
        elseif op == "/"
            new_target = other_val ÷ target
        end
        
        return find_human_value(monkeys, right, new_target)
    end
end

function solve_part1(filename)
    monkeys = parse_monkeys(filename)
    return calculate_monkey(monkeys, "root")
end

function solve_part2(filename)
    monkeys = parse_monkeys(filename)
    
    # Modify root to be an equality check
    root_job = monkeys["root"]
    left, _, right = root_job
    
    # Determine which side contains the human
    if has_human(monkeys, left)
        target = calculate_monkey(monkeys, right)
        return find_human_value(monkeys, left, target)
    else
        target = calculate_monkey(monkeys, left)
        return find_human_value(monkeys, right, target)
    end
end

function main()
    filename = "input.txt"
    
    # Part 1
    part1_result = solve_part1(filename)
    println("Part 1: ", part1_result)
    
    # Part 2
    part2_result = solve_part2(filename)
    println("Part 2: ", part2_result)
end

main()
