function main()
    program = parse.(Int, split(read("input.txt", String), ","))
    input = 5
    output = 0
    i = 1
    while true
        opcode = program[i] % 100
        modes = program[i] ÷ 100
        param1Mode = modes % 10
        modes ÷= 10
        param2Mode = modes % 10
        if opcode == 1
            p1 = getValue(program, i+1, param1Mode)
            p2 = getValue(program, i+2, param2Mode)
            p3 = program[i+3]
            program[p3+1] = p1 + p2
            i += 4
        elseif opcode == 2
            p1 = getValue(program, i+1, param1Mode)
            p2 = getValue(program, i+2, param2Mode)
            p3 = program[i+3]
            program[p3+1] = p1 * p2
            i += 4
        elseif opcode == 3
            program[program[i+1]+1] = input
            i += 2
        elseif opcode == 4
            output = getValue(program, i+1, param1Mode)
            println(output)
            i += 2
        elseif opcode == 5
            p1 = getValue(program, i+1, param1Mode)
            p2 = getValue(program, i+2, param2Mode)
            if p1 != 0
                i = p2+1
            else
                i += 3
            end
        elseif opcode == 6
            p1 = getValue(program, i+1, param1Mode)
            p2 = getValue(program, i+2, param2Mode)
            if p1 == 0
                i = p2+1
            else
                i += 3
            end
        elseif opcode == 7
            p1 = getValue(program, i+1, param1Mode)
            p2 = getValue(program, i+2, param2Mode)
            p3 = program[i+3]+1
            if p1 < p2
                program[p3] = 1
            else
                program[p3] = 0
            end
            i += 4
        elseif opcode == 8
            p1 = getValue(program, i+1, param1Mode)
            p2 = getValue(program, i+2, param2Mode)
            p3 = program[i+3]+1
            if p1 == p2
                program[p3] = 1
            else
                program[p3] = 0
            end
            i += 4
        elseif opcode == 99
            return
        else
            error("Invalid opcode: $opcode")
        end
    end
end

function getValue(program, pos, mode)
    if mode == 0
        return program[program[pos]+1]
    else
        return program[pos]
    end
end

main()