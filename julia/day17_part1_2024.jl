
using Printf

function solve()
    A = 0
    B = 0
    C = 0
    program = Int[]

    open("input.txt", "r") do f
        for line in eachline(f)
            sline = strip(line)
            if isempty(sline)
                continue
            end

            parts = split(sline, ':', limit=2)
            if length(parts) == 2
                key = strip(parts[1])
                value_str = strip(parts[2])

                if key == "Register A"
                    A = parse(Int, value_str)
                elseif key == "Register B"
                    B = parse(Int, value_str)
                elseif key == "Register C"
                    C = parse(Int, value_str)
                elseif key == "Program"
                    if !isempty(value_str)
                         program = [parse(Int, strip(x)) for x in split(value_str, ',')]
                    else
                         program = Int[]
                    end
                end
            end
        end
    end

    function get_combo_val(op::Int)
        if 0 <= op <= 3
            return op
        elseif op == 4
            return A
        elseif op == 5
            return B
        elseif op == 6
            return C
        else
            throw(ArgumentError("invalid combo operand: $op"))
        end
    end

    output_vals = Int[]
    ip = 1

    prog_len = length(program)
    while ip <= prog_len
        opcode = program[ip]

        if ip + 1 > prog_len
            break
        end
        operand = program[ip + 1]

        next_ip = ip + 2

        if opcode == 0
            den = get_combo_val(operand)
            A = (den != 0) ? (A >> den) : 0
        elseif opcode == 1
            B = xor(B, operand)
        elseif opcode == 2
            B = get_combo_val(operand) % 8
        elseif opcode == 3
            if A != 0
                # Adjust jump target for 1-based indexing
                next_ip = operand + 1
            end
        elseif opcode == 4
            B = xor(B, C)
        elseif opcode == 5
            push!(output_vals, get_combo_val(operand) % 8)
        elseif opcode == 6
             den = get_combo_val(operand)
             # Assuming arithmetic right shift similar to Python's // for powers of 2
             B = A >> den
        elseif opcode == 7
             den = get_combo_val(operand)
             C = A >> den
        else
             break
        end
        ip = next_ip
    end

    println(join(string.(output_vals), ","))
end

function main()
    solve()
end

main()
