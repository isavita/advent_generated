
data = open("input.txt") do file
    read(file, String)
end

strs = split(strip(data), ",")
original = parse.(Int, strs)

function execute(memory)
    for i in 1:4:length(memory)
        if memory[i] == 1
            memory[memory[i+3]+1] = memory[memory[i+1]+1] + memory[memory[i+2]+1]
        elseif memory[i] == 2
            memory[memory[i+3]+1] = memory[memory[i+1]+1] * memory[memory[i+2]+1]
        elseif memory[i] == 99
            return memory[1]
        end
    end
    return memory[1]
end

for noun in 0:99
    for verb in 0:99
        memory = copy(original)
        memory[2] = noun
        memory[3] = verb
        if execute(memory) == 19690720
            println(100 * noun + verb)
            break
        end
    end
end
