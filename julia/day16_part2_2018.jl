
struct OP
    a::Char
    b::Char
    action::Char
    name::String
    matchCount::Vector{UInt8}
end

function read_input(filename)
    open(filename, "r") do f
        return readlines(f)
    end
end

function reg_split(text, delimiter)
    return split(text, Regex(delimiter))
end

function str_to_int(s)
    return parse(Int, s)
end

function run_op(op::OP, registers::Vector{Int}, instruction::Vector{UInt8})
    register_cp = copy(registers)
    A = op.a == 'r' ? register_cp[instruction[2] + 1] : instruction[2]
    B = op.b == 'r' ? register_cp[instruction[3] + 1] : instruction[3]
    if op.action == '+'
        register_cp[instruction[4] + 1] = A + B
    elseif op.action == '*'
        register_cp[instruction[4] + 1] = A * B
    elseif op.action == '&'
        register_cp[instruction[4] + 1] = A & B
    elseif op.action == '|'
        register_cp[instruction[4] + 1] = A | B
    elseif op.action == 'a'
        register_cp[instruction[4] + 1] = A
    elseif op.action == '>'
        register_cp[instruction[4] + 1] = A > B ? 1 : 0
    elseif op.action == '='
        register_cp[instruction[4] + 1] = A == B ? 1 : 0
    end
    return register_cp
end

function match(r::Vector{Int}, c::Vector{Int})
    return r == c
end

function test_code(registers::Vector{Int}, result::Vector{Int}, instruction::Vector{UInt8}, opcodes::Vector{OP})
    sum = 0
    for op in opcodes
        if match(result, run_op(op, registers, instruction))
            if !(instruction[1] in op.matchCount)
                push!(op.matchCount, instruction[1])
            end
            sum += 1
        end
    end
    return sum
end

function remove(op::OP, c::UInt8)
    filter!(x -> x != c, op.matchCount)
end

function main()
    lines = read_input("input.txt")
    opcodes = [
        OP('r', 'r', '+', "addr", UInt8[]),
        OP('r', 'v', '+', "addi", UInt8[]),
        OP('r', 'r', '*', "mulr", UInt8[]),
        OP('r', 'v', '*', "muli", UInt8[]),
        OP('r', 'r', '&', "banr", UInt8[]),
        OP('r', 'v', '&', "bani", UInt8[]),
        OP('r', 'r', '|', "borr", UInt8[]),
        OP('r', 'v', '|', "bori", UInt8[]),
        OP('r', 'r', 'a', "setr", UInt8[]),
        OP('v', 'r', 'a', "seti", UInt8[]),
        OP('v', 'r', '>', "gtir", UInt8[]),
        OP('r', 'v', '>', "gtri", UInt8[]),
        OP('r', 'r', '>', "gtrr", UInt8[]),
        OP('v', 'r', '=', "eqir", UInt8[]),
        OP('r', 'v', '=', "eqri", UInt8[]),
        OP('r', 'r', '=', "eqrr", UInt8[])
    ]

    sum = 0
    line_count = 1
    while line_count <= length(lines)
        if startswith(lines[line_count], "Before")
            split = reg_split(lines[line_count], "[^0-9]+")
            registers = [str_to_int(split[i]) for i in 2:5]
            split = reg_split(lines[line_count + 1], "[^0-9]+")
            instruction = [UInt8(str_to_int(split[i])) for i in 1:4]
            split = reg_split(lines[line_count + 2], "[^0-9]+")
            result = [str_to_int(split[i]) for i in 2:5]
            temp_sum = test_code(registers, result, instruction, opcodes)
            if temp_sum >= 3
                sum += 1
            end
            line_count += 4
        else
            break
        end
    end

    ordered_op_codes = Dict{UInt8, OP}()
    while length(ordered_op_codes) < 16
        for op in opcodes
            if length(op.matchCount) == 1
                c = op.matchCount[1]
                ordered_op_codes[c] = op
                for op2 in opcodes
                    remove(op2, c)
                end
            end
        end
    end

    line_count += 2
    r = [0, 0, 0, 0]
    while line_count <= length(lines)
        split = reg_split(lines[line_count], "[^0-9]+")
        instruction = [UInt8(str_to_int(split[i])) for i in 1:4]
        r = run_op(ordered_op_codes[instruction[1]], r, instruction)
        line_count += 1
    end

    println(r[1])
end

main()
