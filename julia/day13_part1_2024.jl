
function solve_machine(ax, ay, bx, by, px, py)
    min_cost = -1
    for a_count in 0:100, b_count in 0:100
        x = ax * a_count + bx * b_count
        y = ay * a_count + by * b_count
        if x == px && y == py
            cost = a_count * 3 + b_count
            if min_cost < 0 || cost < min_cost
                min_cost = cost
            end
        end
    end
    return min_cost
end

function parse_line(s)
    parts = split(strip(s), ",")
    x = parse(Int, replace(strip(parts[1]), r"X\+|Y\+|X=|Y=" => ""))
    y = parse(Int, replace(strip(parts[2]), r"X\+|Y\+|X=|Y=" => ""))
    return x, y
end

function parse_prize(s)
    parts = split(strip(s), ",")
    x = parse(Int, replace(strip(parts[1]), r"X=" => ""))
    y = parse(Int, replace(strip(parts[2]), r"Y=" => ""))
    return x, y
end

function parse_machine(lines)
    ax, ay, bx, by, px, py = 0, 0, 0, 0, 0, 0
    for line in lines
        line = replace(line, "Button A:" => "A:")
        line = replace(line, "Button B:" => "B:")
        line = replace(line, "Prize:" => "P:")
        if startswith(line, "A:")
            ax, ay = parse_line(line[3:end])
        elseif startswith(line, "B:")
            bx, by = parse_line(line[3:end])
        elseif startswith(line, "P:")
            px, py = parse_prize(line[3:end])
        end
    end
    return ax, ay, bx, by, px, py
end

function read_input(filename)
    machines = []
    lines = []
    for line in eachline(filename)
        line = strip(line)
        if isempty(line)
            if !isempty(lines)
                push!(machines, parse_machine(lines))
                lines = []
            end
        else
            push!(lines, line)
        end
    end
    if !isempty(lines)
        push!(machines, parse_machine(lines))
    end
    return machines
end

function main()
    machines = read_input("input.txt")
    results = Int[]
    for (ax, ay, bx, by, px, py) in machines
        cost = solve_machine(ax, ay, bx, by, px, py)
        if cost >= 0
            push!(results, cost)
        end
    end
    if isempty(results)
        println("0 0")
        return
    end
    println("$(length(results)) $(sum(results))")
end

main()
