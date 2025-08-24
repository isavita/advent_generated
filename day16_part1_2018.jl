# Define the function for each opcode
function addr(reg, A, B, C)
    reg[C+1] = reg[A+1] + reg[B+1]
end

function addi(reg, A, B, C)
    reg[C+1] = reg[A+1] + B
end

function mulr(reg, A, B, C)
    reg[C+1] = reg[A+1] * reg[B+1]
end

function muli(reg, A, B, C)
    reg[C+1] = reg[A+1] * B
end

function banr(reg, A, B, C)
    reg[C+1] = reg[A+1] & reg[B+1]
end

function bani(reg, A, B, C)
    reg[C+1] = reg[A+1] & B
end

function borr(reg, A, B, C)
    reg[C+1] = reg[A+1] | reg[B+1]
end

function bori(reg, A, B, C)
    reg[C+1] = reg[A+1] | B
end

function setr(reg, A, B, C)
    reg[C+1] = reg[A+1]
end

function seti(reg, A, B, C)
    reg[C+1] = A
end

function gtir(reg, A, B, C)
    reg[C+1] = A > reg[B+1] ? 1 : 0
end

function gtri(reg, A, B, C)
    reg[C+1] = reg[A+1] > B ? 1 : 0
end

function gtrr(reg, A, B, C)
    reg[C+1] = reg[A+1] > reg[B+1] ? 1 : 0
end

function eqir(reg, A, B, C)
    reg[C+1] = A == reg[B+1] ? 1 : 0
end

function eqri(reg, A, B, C)
    reg[C+1] = reg[A+1] == B ? 1 : 0
end

function eqrr(reg, A, B, C)
    reg[C+1] = reg[A+1] == reg[B+1] ? 1 : 0
end

# All opcodes in a list
opcodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

# Read input and process each sample
function process_samples(filename)
    lines = readlines(filename)
    sample_count = 0
    i = 1
    while i <= length(lines)
        if startswith(lines[i], "Before:")
            before = parse.(Int, match(r"\[(\d+), (\d+), (\d+), (\d+)\]", lines[i]).captures)
            instruction = parse.(Int, split(lines[i+1]))
            after = parse.(Int, match(r"\[(\d+), (\d+), (\d+), (\d+)\]", lines[i+2]).captures)
            
            match_count = 0
            for opcode in opcodes
                reg = copy(before)
                opcode(reg, instruction[2], instruction[3], instruction[4])
                if reg == after
                    match_count += 1
                end
            end

            if match_count >= 3
                sample_count += 1
            end

            i += 4  # Move to the next block of samples
        else
            i += 1
        end
    end
    return sample_count
end

# Main execution
filename = "input.txt"
println("Number of samples behaving like three or more opcodes: ", process_samples(filename))