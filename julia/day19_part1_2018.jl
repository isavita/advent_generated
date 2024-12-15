
function solve()
    lines = readlines("input.txt")
    ip_reg = parse(Int, split(lines[1])[2])
    instructions = map(lines[2:end]) do line
        parts = split(line)
        op = parts[1]
        args = parse.(Int, parts[2:end])
        (op, args)
    end

    registers = zeros(Int, 6)
    ip = 0

    while 0 <= ip < length(instructions)
        registers[ip_reg + 1] = ip
        op, args = instructions[ip + 1]
        
        if op == "addr"
            registers[args[3] + 1] = registers[args[1] + 1] + registers[args[2] + 1]
        elseif op == "addi"
            registers[args[3] + 1] = registers[args[1] + 1] + args[2]
        elseif op == "mulr"
            registers[args[3] + 1] = registers[args[1] + 1] * registers[args[2] + 1]
        elseif op == "muli"
            registers[args[3] + 1] = registers[args[1] + 1] * args[2]
        elseif op == "banr"
            registers[args[3] + 1] = registers[args[1] + 1] & registers[args[2] + 1]
        elseif op == "bani"
            registers[args[3] + 1] = registers[args[1] + 1] & args[2]
        elseif op == "borr"
            registers[args[3] + 1] = registers[args[1] + 1] | registers[args[2] + 1]
        elseif op == "bori"
            registers[args[3] + 1] = registers[args[1] + 1] | args[2]
        elseif op == "setr"
            registers[args[3] + 1] = registers[args[1] + 1]
        elseif op == "seti"
            registers[args[3] + 1] = args[1]
        elseif op == "gtir"
            registers[args[3] + 1] = args[1] > registers[args[2] + 1] ? 1 : 0
        elseif op == "gtri"
            registers[args[3] + 1] = registers[args[1] + 1] > args[2] ? 1 : 0
        elseif op == "gtrr"
            registers[args[3] + 1] = registers[args[1] + 1] > registers[args[2] + 1] ? 1 : 0
        elseif op == "eqir"
            registers[args[3] + 1] = args[1] == registers[args[2] + 1] ? 1 : 0
        elseif op == "eqri"
            registers[args[3] + 1] = registers[args[1] + 1] == args[2] ? 1 : 0
        elseif op == "eqrr"
            registers[args[3] + 1] = registers[args[1] + 1] == registers[args[2] + 1] ? 1 : 0
        end
        
        ip = registers[ip_reg + 1]
        ip += 1
    end
    println(registers[1])
end

solve()
