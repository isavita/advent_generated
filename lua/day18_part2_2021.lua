function parseSnailNumber(input)
    input = input:gsub("%s+", "")
    if input:sub(1, 1) ~= '[' then
        return {value = tonumber(input)}
    end

    local balance, splitIndex = 0, 0
    for i = 2, #input - 1 do
        local char = input:sub(i, i)
        if char == '[' then
            balance = balance + 1
        elseif char == ']' then
            balance = balance - 1
        elseif char == ',' and balance == 0 then
            splitIndex = i
            break
        end
    end

    local left = parseSnailNumber(input:sub(2, splitIndex - 1))
    local right = parseSnailNumber(input:sub(splitIndex + 1, #input - 1))
    return {left = left, right = right}
end

function isRegular(sn)
    return sn.value ~= nil
end

function add(sn1, sn2)
    local newNumber = {left = deepcopy(sn1), right = deepcopy(sn2)}
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
        sn.left, sn.right = nil, nil
        sn.value = 0
        return true, leftValue, rightValue
    end

    local exploded, leftValue, rightValue = explode(sn.left, depth + 1)
    if exploded then
        if rightValue > 0 and sn.right then
            addLeft(sn.right, rightValue)
        end
        return true, leftValue, 0
    end

    exploded, leftValue, rightValue = explode(sn.right, depth + 1)
    if exploded then
        if leftValue > 0 and sn.left then
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
            sn.left = {value = math.floor(sn.value / 2)}
            sn.right = {value = math.ceil(sn.value / 2)}
            sn.value = nil
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

function deepcopy(orig)
    if isRegular(orig) then
        return {value = orig.value}
    end
    return {left = deepcopy(orig.left), right = deepcopy(orig.right)}
end

local file = io.open("input.txt", "r")
local snailNumbers = {}
for line in file:lines() do
    table.insert(snailNumbers, parseSnailNumber(line))
end
file:close()

local largestMagnitude = 0
for i, a in ipairs(snailNumbers) do
    for j, b in ipairs(snailNumbers) do
        if i ~= j then
            local sum1 = magnitude(add(deepcopy(a), deepcopy(b)))
            local sum2 = magnitude(add(deepcopy(b), deepcopy(a)))
            largestMagnitude = math.max(largestMagnitude, sum1, sum2)
        end
    end
end

print(largestMagnitude)