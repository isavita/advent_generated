function solve(input)
    lines = parseInput(input)
    total = 0

    for line in lines
        total += doMaths(line, calcFlatSlicePart)
    end

    return total
end

function parseInput(input)
    lines = split(input, "\n")
    ans = [split(replace(l, " " => ""), "") for l in lines]
    return ans
end

function doMaths(input, flatteningFunc)
    stackOpenIndices = Int[]
    stackFlattened = String[]

    for i in 1:length(input)
        push!(stackFlattened, input[i])

        if input[i] == "("
            push!(stackOpenIndices, length(stackFlattened))
        elseif input[i] == ")"
            openIndex = pop!(stackOpenIndices)
            sliToFlatten = stackFlattened[openIndex+1:end-1]
            stackFlattened[openIndex] = flatteningFunc(sliToFlatten)
            stackFlattened = stackFlattened[1:openIndex]
        end
    end

    return parse(Int, flatteningFunc(stackFlattened))
end

function calcFlatSlicePart(input)
    for v in input
        if v == "(" || v == ")"
            error("unexpected paren in flat input, $input")
        end
    end

    while true
        found = false
        for i in 2:2:length(input)-1
            if input[i] == "+"
                toLeft = input[i-1]
                toRight = input[i+1]
                if isNum(toLeft) && isNum(toRight)
                    input[i-1] = addStrings(toLeft, toRight)
                    deleteat!(input, i:i+1)
                    found = true
                    break
                end
            end
        end
        !found && break
    end

    while true
        found = false
        for i in 2:2:length(input)-1
            if input[i] == "*"
                toLeft = input[i-1]
                toRight = input[i+1]
                if isNum(toLeft) && isNum(toRight)
                    input[i-1] = multiplyStrings(toLeft, toRight)
                    deleteat!(input, i:i+1)
                    found = true
                    break
                end
            end
        end
        !found && break
    end

    return input[1]
end

function isNum(str)
    return match(r"[0-9]", str) !== nothing
end

function addStrings(strs...)
    return string(sum(parse(Int, str) for str in strs))
end

function multiplyStrings(strs...)
    return string(prod(parse(Int, str) for str in strs))
end

input = read("input.txt", String)
input = strip(input)
ans = solve(input)
println(ans)