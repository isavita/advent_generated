
using DelimitedFiles

operations = Dict(
    "addr" => (r, a, b) -> r[a+1] + r[b+1],
    "addi" => (r, a, b) -> r[a+1] + b,
    "mulr" => (r, a, b) -> r[a+1] * r[b+1],
    "muli" => (r, a, b) -> r[a+1] * b,
    "banr" => (r, a, b) -> r[a+1] & r[b+1],
    "bani" => (r, a, b) -> r[a+1] & b,
    "borr" => (r, a, b) -> r[a+1] | r[b+1],
    "bori" => (r, a, b) -> r[a+1] | b,
    "setr" => (r, a, b) -> r[a+1],
    "seti" => (r, a, b) -> a,
    "gtir" => (r, a, b) -> a > r[b+1] ? 1 : 0,
    "gtri" => (r, a, b) -> r[a+1] > b ? 1 : 0,
    "gtrr" => (r, a, b) -> r[a+1] > r[b+1] ? 1 : 0,
    "eqir" => (r, a, b) -> a == r[b+1] ? 1 : 0,
    "eqri" => (r, a, b) -> r[a+1] == b ? 1 : 0,
    "eqrr" => (r, a, b) -> r[a+1] == r[b+1] ? 1 : 0
)

function load_program(lines)
    program = []
    ip_register = 0
    for line in lines
        if startswith(line, "#ip")
            ip_register = parse(Int, split(line)[2])
            continue
        end
        parts = split(line)
        op = operations[parts[1]]
        a, b, c = parse.(Int, parts[2:4])
        push!(program, (r) -> r[c+1] = op(r, a, b))
    end
    return ip_register, program
end

function run_program(ip_register, program, registers, max_cycles)
    ip = 0
    cycles = 0
    while 0 <= ip < length(program)
        registers[ip_register+1] = ip
        program[ip+1](registers)
        ip = registers[ip_register+1] + 1
        cycles += 1
        if max_cycles > 0 && cycles >= max_cycles
            break
        end
    end
    return registers
end

function main()
    lines = filter(!isempty, readlines("input.txt"))
    ip_register, program = load_program(lines)
    registers = [1, 0, 0, 0, 0, 0]
    registers = run_program(ip_register, program, registers, 1000)
    n = maximum(registers)
    total = sum(i for i in 1:n if n % i == 0)
    println(total)
end

main()
