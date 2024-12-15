
function solve()
    offset = 10000000000000
    machines = read_input("input.txt")
    for m in machines
        m.px += offset
        m.py += offset
    end
    results = Int64[]
    for m in machines
        cost = solve_machine(m)
        if cost >= 0
            push!(results, cost)
        end
    end
    if isempty(results)
        println("0 0")
        return
    end
    count = length(results)
    sum_val = sum(results)
    println("$count $sum_val")
end

function read_input(filename)
    machines = []
    lines = String[]
    open(filename, "r") do file
        for line in eachline(file)
            line = strip(line)
            if isempty(line)
                if !isempty(lines)
                    m = parse_machine(lines)
                    push!(machines, m)
                    lines = String[]
                end
            else
                push!(lines, line)
            end
        end
        if !isempty(lines)
            m = parse_machine(lines)
            push!(machines, m)
        end
    end
    return machines
end

mutable struct Machine
    ax::Int64
    ay::Int64
    bx::Int64
    by::Int64
    px::Int64
    py::Int64
end

function parse_machine(lines)
    m = Machine(0, 0, 0, 0, 0, 0)
    for l in lines
        l = replace(l, "Button A:" => "A:")
        l = replace(l, "Button B:" => "B:")
        l = replace(l, "Prize:" => "P:")
        if startswith(l, "A:")
            m.ax, m.ay = parse_line(l[3:end])
        elseif startswith(l, "B:")
            m.bx, m.by = parse_line(l[3:end])
        elseif startswith(l, "P:")
            m.px, m.py = parse_prize(l[3:end])
        end
    end
    return m
end

function parse_line(s)
    parts = split(strip(s), ",")
    x = parse_val(parts[1])
    y = parse_val(parts[2])
    return x, y
end

function parse_prize(s)
    parts = split(strip(s), ",")
    x = parse_val_prize(parts[1])
    y = parse_val_prize(parts[2])
    return x, y
end

function parse_val(s)
    s = strip(s)
    s = replace(s, r"^X\+" => "")
    s = replace(s, r"^Y\+" => "")
    s = replace(s, r"^X=" => "")
    s = replace(s, r"^Y=" => "")
    return parse(Int64, s)
end

function parse_val_prize(s)
    s = strip(s)
    s = replace(s, r"^X=" => "")
    s = replace(s, r"^Y=" => "")
    return parse(Int64, s)
end

function solve_machine(m::Machine)
    D = m.ax * m.by - m.ay * m.bx
    if D == 0
        return -1
    end
    numA = m.px * m.by - m.py * m.bx
    numB = -m.px * m.ay + m.py * m.ax
    if numA % D != 0 || numB % D != 0
        return -1
    end
    a = numA ÷ D
    b = numB ÷ D
    if a < 0 || b < 0
        return -1
    end
    return 3 * a + b
end

solve()
