
function readFile(path)
    local file = io.open(path, "r")
    local content = file:read("*all")
    file:close()
    return content
end

function elephant(input)
    local startingElves = tonumber(readFile("input.txt"))
    local root = {elfNum = 1, presents = 1}
    local iter = root

    for i = 2, startingElves do
        iter.next = {elfNum = i, presents = 1}
        iter = iter.next
    end
    iter.next = root

    local isOddLength = startingElves % 2 == 1
    local beforeAcross = root
    for i = 1, startingElves/2 - 1 do
        beforeAcross = beforeAcross.next
    end

    while root.next ~= root do
        root.presents = root.presents + beforeAcross.next.presents
        beforeAcross.next = beforeAcross.next.next

        if isOddLength then
            beforeAcross = beforeAcross.next
        end
        isOddLength = not isOddLength
        root = root.next
    end

    return root.elfNum
end

local input = readFile("input.txt")
local ans = elephant(input)
print(ans)
