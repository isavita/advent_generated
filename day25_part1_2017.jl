# Define a function to parse the input and extract the machine's specifications
function parse_input(filename)
    states = Dict()
    start_state = ""
    steps = 0
    current_section = nothing
    state_name = ""

    open(filename, "r") do file
        for line in eachline(file)
            line = strip(line)
            if isempty(line)
                continue
            elseif occursin("Begin in state", line)
                start_state = match(r"Begin in state ([A-Z])\.", line).captures[1]
            elseif occursin("Perform a diagnostic checksum after", line)
                steps = parse(Int, match(r"Perform a diagnostic checksum after (\d+) steps\.", line).captures[1])
            elseif occursin("In state", line)
                state_name = match(r"In state ([A-Z]):", line).captures[1]
                states[state_name] = Dict()
            elseif occursin("If the current value is", line)
                value = parse(Int, match(r"If the current value is (\d):", line).captures[1])
                states[state_name][value] = Dict()
                current_section = states[state_name][value]
            else
                if occursin("Write the value", line)
                    current_section["write"] = parse(Int, match(r"Write the value (\d).", line).captures[1])
                elseif occursin("Move one slot to the", line)
                    direction = match(r"Move one slot to the (right|left).", line).captures[1]
                    current_section["move"] = (direction == "right" ? 1 : -1)
                elseif occursin("Continue with state", line)
                    current_section["next_state"] = match(r"Continue with state ([A-Z]).", line).captures[1]
                end
            end
        end
    end

    return start_state, steps, states
end

# Define a function to run the Turing machine
function run_turing_machine(start_state, steps, states)
    tape = Dict{Int, Int}()
    cursor = 0
    current_state = start_state

    for _ in 1:steps
        current_value = get(tape, cursor, 0)
        instructions = states[current_state][current_value]
        tape[cursor] = instructions["write"]
        cursor += instructions["move"]
        current_state = instructions["next_state"]
    end

    # Calculate the diagnostic checksum
    checksum = sum(values(tape))
    return checksum
end

# Main execution block
function main()
    start_state, steps, states = parse_input("input.txt")
    checksum = run_turing_machine(start_state, steps, states)
    println("Diagnostic checksum: ", checksum)
end

main()