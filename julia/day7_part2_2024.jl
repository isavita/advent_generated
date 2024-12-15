
function concat(a, b)
    parse(Int, string(a) * string(b))
end

function can_produce(target, nums, idx, value)
    if idx > length(nums)
        return value == target
    end
    n = nums[idx]
    return can_produce(target, nums, idx + 1, value + n) ||
           can_produce(target, nums, idx + 1, value * n) ||
           can_produce(target, nums, idx + 1, concat(value, n))
end

function solve()
    total = 0
    open("input.txt") do f
        for line in eachline(f)
            line = strip(line)
            if isempty(line)
                continue
            end
            parts = split(line, ":")
            target = parse(Int, strip(parts[1]))
            num_strs = split(strip(parts[2]))
            if isempty(num_strs)
                continue
            end
            nums = parse.(Int, num_strs)
            if length(nums) == 1
                if nums[1] == target
                    total += target
                end
                continue
            end
            if can_produce(target, nums, 2, nums[1])
                total += target
            end
        end
    end
    println(total)
end

solve()
