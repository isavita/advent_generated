function readSnailNumber(input)
    if input:sub(1, 1) ~= "[" then
        return {value = tonumber(input)}
    end

    local balance = 0
    local splitIndex = 0
    for i = 2, #input - 1 do
        local char = input:sub(i, i)
        if char == "[" then
            balance = balance + 1
        elseif char == "]" then
            balance = balance - 1
        elseif char == "," and balance == 0 then
            splitIndex = i
            break
        end
    end

    local left = readSnailNumber(input:sub(2, splitIndex - 1))
    local right = readSnailNumber(input:sub(splitIndex + 1, #input - 1))
    return {left = left, right = right}
end

function isRegular(sn)
    return sn.left == nil and sn.right == nil
end

function add(sn, other)
    local newNumber = {left = sn, right = other}
    return reduce(newNumber)
end

function reduce(sn)
    while true do
        local exploded, _, _ = explode(sn, 0)
        if exploded then
            goto continue
        end
        if not split(sn) then
            break
        end
        ::continue::
    end
    return sn
end

function explode(sn, depth)
    if isRegular(sn) then
        return false, 0, 0
    end

    if depth == 4 then
        local leftValue = sn.left.value
        local rightValue = sn.right.value
        sn.left = nil
        sn.right = nil
        sn.value = 0
        return true, leftValue, rightValue
    end

    local exploded, leftValue, rightValue = explode(sn.left, depth + 1)
    if exploded then
        if rightValue > 0 and sn.right ~= nil then
            addLeft(sn.right, rightValue)
        end
        return true, leftValue, 0
    end

    exploded, leftValue, rightValue = explode(sn.right, depth + 1)
    if exploded then
        if leftValue > 0 and sn.left ~= nil then
            addRight(sn.left, leftValue)
        end
        return true, 0, rightValue
    end

    return false, 0, 0
end

function addLeft(sn, value)
    if isRegular(sn) then
        sn.value = sn.value + value
    else
        addLeft(sn.left, value)
    end
end

function addRight(sn, value)
    if isRegular(sn) then
        sn.value = sn.value + value
    else
        addRight(sn.right, value)
    end
end

function split(sn)
    if isRegular(sn) then
        if sn.value >= 10 then
            sn.left = {value = sn.value // 2}
            sn.right = {value = (sn.value + 1) // 2}
            sn.value = -1
            return true
        end
        return false
    end
    return split(sn.left) or split(sn.right)
end

function magnitude(sn)
    if isRegular(sn) then
        return sn.value
    end
    return 3 * magnitude(sn.left) + 2 * magnitude(sn.right)
end

local snailNumbers = {}
for line in io.lines("input.txt") do
    table.insert(snailNumbers, readSnailNumber(line))
end

if #snailNumbers == 0 then
    print("No snailfish numbers found in the file.")
    return
end

local result = snailNumbers[1]
for i = 2, #snailNumbers do
    result = add(result, snailNumbers[i])
end

print(magnitude(result))