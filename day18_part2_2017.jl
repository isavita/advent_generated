function getValue(arg, registers)
    val = tryparse(Int, arg)
    if val !== nothing
        return val
    end
    return registers[arg]
end

function main()
    instructions = []
    open("input.txt") do file
        for line in eachline(file)
            push!(instructions, split(line))
        end
    end

    registers0 = Dict("p" => 0)
    registers1 = Dict("p" => 1)
    queue0 = Int[]
    queue1 = Int[]
    sendCount1 = 0
    i0, i1 = 1, 1
    deadlock0, deadlock1 = false, false

    while !(deadlock0 && deadlock1)
        deadlock0, deadlock1 = true, true

        # Program 0
        while i0 <= length(instructions)
            instruction = instructions[i0]
            cmd = instruction[1]
            arg1 = instruction[2]
            if cmd == "snd"
                push!(queue1, getValue(arg1, registers0))
            elseif cmd == "set"
                registers0[arg1] = getValue(instruction[3], registers0)
            elseif cmd == "add"
                registers0[arg1] += getValue(instruction[3], registers0)
            elseif cmd == "mul"
                registers0[arg1] *= getValue(instruction[3], registers0)
            elseif cmd == "mod"
                registers0[arg1] %= getValue(instruction[3], registers0)
            elseif cmd == "rcv"
                if isempty(queue0)
                    break
                end
                registers0[arg1] = popfirst!(queue0)
            elseif cmd == "jgz"
                if getValue(arg1, registers0) > 0
                    i0 += getValue(instruction[3], registers0)
                    continue
                end
            end
            i0 += 1
            deadlock0 = false
        end

        # Program 1
        while i1 <= length(instructions)
            instruction = instructions[i1]
            cmd = instruction[1]
            arg1 = instruction[2]
            if cmd == "snd"
                push!(queue0, getValue(arg1, registers1))
                sendCount1 += 1
            elseif cmd == "set"
                registers1[arg1] = getValue(instruction[3], registers1)
            elseif cmd == "add"
                registers1[arg1] += getValue(instruction[3], registers1)
            elseif cmd == "mul"
                registers1[arg1] *= getValue(instruction[3], registers1)
            elseif cmd == "mod"
                registers1[arg1] %= getValue(instruction[3], registers1)
            elseif cmd == "rcv"
                if isempty(queue1)
                    break
                end
                registers1[arg1] = popfirst!(queue1)
            elseif cmd == "jgz"
                if getValue(arg1, registers1) > 0
                    i1 += getValue(instruction[3], registers1)
                    continue
                end
            end
            i1 += 1
            deadlock1 = false
        end
    end

    println(sendCount1)
end

main()