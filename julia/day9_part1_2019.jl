
function main()
    program = readlines("input.txt")[1]
    memory = Dict{Int, Int}()
    for (i, s) in enumerate(split(program, ","))
        memory[i - 1] = parse(Int, s)
    end
    println(run_intcode(memory))
end

function run_intcode(memory)
    output, ip, relative_base = 0, 0, 0

    while true
        opcode = memory[ip] % 100
        modes = div(memory[ip], 100)

        get_param(offset) = begin
            mode = div(modes, 10^(offset - 1)) % 10
            param = memory[ip + offset]
            mode == 0 ? get(memory, param, 0) : mode == 1 ? param : get(memory, relative_base + param, 0)
        end

        set_param(offset, value) = begin
            mode = div(modes, 10^(offset - 1)) % 10
            param = memory[ip + offset]
            if mode == 0
                memory[param] = value
            else
                memory[relative_base + param] = value
            end
        end

        if opcode == 1
            set_param(3, get_param(1) + get_param(2))
            ip += 4
        elseif opcode == 2
            set_param(3, get_param(1) * get_param(2))
            ip += 4
        elseif opcode == 3
            set_param(1, 1)
            ip += 2
        elseif opcode == 4
            output = get_param(1)
            ip += 2
        elseif opcode == 5
            ip = get_param(1) != 0 ? get_param(2) : ip + 3
        elseif opcode == 6
            ip = get_param(1) == 0 ? get_param(2) : ip + 3
        elseif opcode == 7
            set_param(3, get_param(1) < get_param(2) ? 1 : 0)
            ip += 4
        elseif opcode == 8
            set_param(3, get_param(1) == get_param(2) ? 1 : 0)
            ip += 4
        elseif opcode == 9
            relative_base += get_param(1)
            ip += 2
        elseif opcode == 99
            return output
        else
            error("unknown opcode: $opcode")
        end
    end
end

main()
