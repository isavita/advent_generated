
using Base.Iterators: enumerate

function main()
    data = read("input.txt", String)
    lines = split(data, r"\r?\n", keepempty=false)
    if isempty(lines)
        println("Grand total: 0")
        return
    end

    maxw = maximum(length.(lines))
    issep = trues(maxw)

    for x in 1:maxw
        for line in lines
            if x <= length(line) && !isspace(line[x])
                issep[x] = false
                break
            end
        end
    end

    total = BigInt(0)
    inblock = false
    start = 0

    for x in 1:maxw
        if !issep[x]
            if !inblock
                inblock = true
                start = x
            end
        else
            if inblock
                total += process_block(lines, start, x - 1)
                inblock = false
            end
        end
    end
    if inblock
        total += process_block(lines, start, maxw)
    end

    println("Grand total: $total")
end

function process_block(lines, s, e)
    op = ""
    nums = BigInt[]
    for line in lines
        if s <= length(line)
            seg = strip(line[s:min(e, length(line))])
            if !isempty(seg)
                if seg == "+" || seg == "*"
                    op = seg
                else
                    try
                        push!(nums, parse(BigInt, seg))
                    catch
                    end
                end
            end
        end
    end
    if isempty(nums)
        return BigInt(0)
    elseif op == "+"
        return sum(nums)
    elseif op == "*"
        prod = one(BigInt)
        for n in nums
            prod *= n
        end
        return prod
    elseif length(nums) == 1
        return nums[1]
    else
        return BigInt(0)
    end
end

main()
