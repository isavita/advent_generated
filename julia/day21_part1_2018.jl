
function solve()
    input = read("input.txt", String)
    lines = split(input, "\n")
    instruction_pointer = parse(Int, lines[1][5:end])
    instructions = map(lines[2:end]) do l
        parts = split(l)
        (parts[1], parse.(Int, parts[2:end])...)
    end
    registers = zeros(Int, 6)
    while true
        inst_index = registers[instruction_pointer+1] + 1
        if inst_index > length(instructions)
            break
        end
        inst = instructions[inst_index]
        opcode = inst[1]
        a, b, c = inst[2:end]
        if opcode == "addr"
            registers[c+1] = registers[a+1] + registers[b+1]
        elseif opcode == "addi"
            registers[c+1] = registers[a+1] + b
        elseif opcode == "mulr"
            registers[c+1] = registers[a+1] * registers[b+1]
        elseif opcode == "muli"
            registers[c+1] = registers[a+1] * b
        elseif opcode == "banr"
            registers[c+1] = registers[a+1] & registers[b+1]
        elseif opcode == "bani"
            registers[c+1] = registers[a+1] & b
        elseif opcode == "borr"
            registers[c+1] = registers[a+1] | registers[b+1]
        elseif opcode == "bori"
            registers[c+1] = registers[a+1] | b
        elseif opcode == "setr"
            registers[c+1] = registers[a+1]
        elseif opcode == "seti"
            registers[c+1] = a
        elseif opcode == "gtir"
            registers[c+1] = a > registers[b+1] ? 1 : 0
        elseif opcode == "gtri"
            registers[c+1] = registers[a+1] > b ? 1 : 0
        elseif opcode == "gtrr"
            registers[c+1] = registers[a+1] > registers[b+1] ? 1 : 0
        elseif opcode == "eqir"
            registers[c+1] = a == registers[b+1] ? 1 : 0
        elseif opcode == "eqri"
            registers[c+1] = registers[a+1] == b ? 1 : 0
        elseif opcode == "eqrr"
            registers[c+1] = registers[a+1] == registers[b+1] ? 1 : 0
        end
        registers[instruction_pointer+1] += 1
        if registers[instruction_pointer+1] == 28
            break
        end
    end
    registers[6]
end

println(solve())
