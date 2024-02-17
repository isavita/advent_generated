
open("input.txt") do file
    registers = Dict{String, Int}()
    highestValue = 0

    for line in eachline(file)
        parts = split(line)
        reg = parts[1]
        op = parts[2]
        amount = parse(Int, parts[3])
        condReg = parts[5]
        condOp = parts[6]
        condVal = parse(Int, parts[7])

        if !haskey(registers, reg)
            registers[reg] = 0
        end

        if !haskey(registers, condReg)
            registers[condReg] = 0
        end

        cond = false
        if condOp == ">"
            cond = registers[condReg] > condVal
        elseif condOp == ">="
            cond = registers[condReg] >= condVal
        elseif condOp == "<"
            cond = registers[condReg] < condVal
        elseif condOp == "<="
            cond = registers[condReg] <= condVal
        elseif condOp == "=="
            cond = registers[condReg] == condVal
        elseif condOp == "!="
            cond = registers[condReg] != condVal
        end

        if cond
            if op == "inc"
                registers[reg] += amount
            elseif op == "dec"
                registers[reg] -= amount
            end

            if registers[reg] > highestValue
                highestValue = registers[reg]
            end
        end
    end

    println(highestValue)
end
