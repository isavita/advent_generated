
using Printf

function is_register(x::AbstractString)
    return length(x) == 1 && x[1] in "abcd"
end

function get_value(x::AbstractString, registers::Dict{Char,Int})
    if is_register(x)
        return registers[x[1]]
    else
        return parse(Int, x)
    end
end

function execute_program(instructions_in::Vector{String}, registers::Dict{Char,Int})
    instructions = copy(instructions_in) # Make mutable copy for tgl
    n = length(instructions)
    i = 1

    while i <= n
        # Optimization Check for multiplication pattern
        if i + 5 <= n
            p1_parts = split(instructions[i])
            p2_parts = split(instructions[i+1])
            p3_parts = split(instructions[i+2])
            p4_parts = split(instructions[i+3])
            p5_parts = split(instructions[i+4])
            p6_parts = split(instructions[i+5])

            if p1_parts[1] == "cpy" && length(p1_parts) == 3 &&
               p2_parts[1] == "inc" && length(p2_parts) == 2 &&
               p3_parts[1] == "dec" && length(p3_parts) == 2 &&
               p4_parts[1] == "jnz" && length(p4_parts) == 3 &&
               p5_parts[1] == "dec" && length(p5_parts) == 2 &&
               p6_parts[1] == "jnz" && length(p6_parts) == 3

                cpy_x, cpy_y = p1_parts[2], p1_parts[3]
                inc_a = p2_parts[2]
                dec_c = p3_parts[2]
                jnz_c, jnz_c_offset_str = p4_parts[2], p4_parts[3]
                dec_d = p5_parts[2]
                jnz_d, jnz_d_offset_str = p6_parts[2], p6_parts[3]

                jnz_c_offset = tryparse(Int, jnz_c_offset_str)
                jnz_d_offset = tryparse(Int, jnz_d_offset_str)

                if jnz_c_offset !== nothing && jnz_d_offset !== nothing &&
                   is_register(cpy_y) && is_register(inc_a) && is_register(dec_c) && is_register(jnz_c) && is_register(dec_d) && is_register(jnz_d) &&
                   inc_a == "a" && dec_c == cpy_y && jnz_c == cpy_y && jnz_c_offset == -2 &&
                   dec_d == "d" && jnz_d == "d" && jnz_d_offset == -5

                    # Apply optimized multiplication
                    val_cpy_x = get_value(cpy_x, registers)
                    val_d = get_value(dec_d, registers) # Use dec_d which must be 'd'
                    registers['a'] += val_cpy_x * val_d
                    registers[cpy_y[1]] = 0
                    registers['d'] = 0
                    i += 6
                    continue # Skip normal execution
                end
            end
        end

        # Normal instruction execution
        parts = split(instructions[i])
        cmd = parts[1]

        if cmd == "tgl" && length(parts) == 2
            x = parts[2]
            offset = get_value(x, registers)
            target_idx = i + offset
            if 1 <= target_idx <= n
                target_instruction = instructions[target_idx]
                target_parts = split(target_instruction)
                target_cmd = target_parts[1]

                if length(target_parts) == 2 # inc, dec, tgl etc. (one argument)
                    if target_cmd == "inc"
                        target_parts[1] = "dec"
                    else
                        target_parts[1] = "inc"
                    end
                elseif length(target_parts) == 3 # jnz, cpy (two arguments)
                    if target_cmd == "jnz"
                        target_parts[1] = "cpy"
                    else
                        target_parts[1] = "jnz"
                    end
                end
                instructions[target_idx] = join(target_parts, " ")
            end
            i += 1
        elseif cmd == "cpy" && length(parts) == 3
            x, y = parts[2], parts[3]
            if is_register(y)
                registers[y[1]] = get_value(x, registers)
            end
            i += 1
        elseif cmd == "inc" && length(parts) == 2
            x = parts[1+1] # parts[2]
            if is_register(x)
                registers[x[1]] += 1
            end
            i += 1
        elseif cmd == "dec" && length(parts) == 2
            x = parts[2]
            if is_register(x)
                registers[x[1]] -= 1
            end
            i += 1
        elseif cmd == "jnz" && length(parts) == 3
            x, y = parts[2], parts[3]
            val_x = get_value(x, registers)
            if val_x != 0
                jump = get_value(y, registers)
                i += jump
            else
                i += 1
            end
        else
             # Skip invalid or unhandled instruction (like Python version)
            i += 1
        end
    end
end

function main()
    instructions = readlines("input.txt")
    registers = Dict{Char,Int}('a' => 12, 'b' => 0, 'c' => 0, 'd' => 0)
    execute_program(instructions, registers)
    println(registers['a'])
end

main()
