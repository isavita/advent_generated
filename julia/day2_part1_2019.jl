
inputData = open("input.txt") do file
    parse.(Int, split(readline(file), ","))
end

inputData[2] = 12
inputData[3] = 2

function executeProgram(data)
    for i in 1:4:length(data)-3
        pos1 = data[i+1] + 1
        pos2 = data[i+2] + 1
        pos3 = data[i+3] + 1
        if data[i] == 1
            data[pos3] = data[pos1] + data[pos2]
        elseif data[i] == 2
            data[pos3] = data[pos1] * data[pos2]
        elseif data[i] == 99
            return data[1]
        else
            error("Invalid opcode")
        end
    end
    return data[1]
end

result = executeProgram(inputData)
println(result)
