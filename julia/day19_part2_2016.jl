function elephant(input::String)
    startingElves = parse(Int, input)
    root = LLNode(1, 1, nothing)
    iter = root
    for i = 2:startingElves
        iter.next = LLNode(i, 1, nothing)
        iter = iter.next
    end
    iter.next = root

    isOddLength = startingElves % 2 == 1
    beforeAcross = root
    for i = 1:div(startingElves, 2) - 1
        beforeAcross = beforeAcross.next
    end

    while root.next !== root
        root.presents += beforeAcross.next.presents

        beforeAcross.next = beforeAcross.next.next

        if isOddLength
            beforeAcross = beforeAcross.next
        end
        isOddLength = !isOddLength
        root = root.next
    end

    return root.elfNum
end

mutable struct LLNode
    elfNum::Int
    presents::Int
    next::Union{LLNode, Nothing}
end

input = read("input.txt", String)
println(elephant(input))