function parse_instruction(instruction)
    parts = split(instruction)
    (parts[1], parts[2], length(parts) > 2 ? parts[3] : nothing)
end

function resolve_value(registers, value)
    if occursin(r"^-?\d+$", value)
        parse(Int, value)
    else
        get(registers, value, 0)
    end
end

function process_instructions(filename)
    instructions = readlines(filename)
    registers = Dict{String, Int}()
    pc = 1
    mul_count = 0

    while pc >= 1 && pc <= length(instructions)
        instruction, x, y = parse_instruction(instructions[pc])
        y_val = y !== nothing ? resolve_value(registers, y) : 0

        if instruction == "set"
            registers[x] = y_val
        elseif instruction == "sub"
            registers[x] = get(registers, x, 0) - y_val
        elseif instruction == "mul"
            registers[x] = get(registers, x, 0) * y_val
            mul_count += 1
        elseif instruction == "jnz"
            x_val = resolve_value(registers, x)
            if x_val != 0
                pc += y_val
                continue
            end
        end
        pc += 1
    end

    mul_count
end

# Main execution block
const INPUT_FILE = "input.txt"
mul_invocations = process_instructions(INPUT_FILE)
println("The 'mul' instruction was invoked $mul_invocations times.")