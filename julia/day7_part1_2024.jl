
function can_produce(target::Int, nums::Vector{Int}, idx::Int, current::Int, memo::Dict{Tuple{Int, Int}, Bool})
    if idx > length(nums)
        return current == target
    end
    key = (idx, current)
    if haskey(memo, key)
        return memo[key]
    end
    plus = can_produce(target, nums, idx + 1, current + nums[idx], memo)
    mul = can_produce(target, nums, idx + 1, current * nums[idx], memo)
    memo[key] = plus || mul
    return memo[key]
end

function solve()
    total = 0
    open("input.txt", "r") do file
        for line in eachline(file)
            if isempty(line)
                continue
            end
            parts = split(line, ":")
            target = parse(Int, strip(parts[1]))
            num_strs = split(strip(parts[2]))
            nums = parse.(Int, num_strs)
            if length(nums) == 1
                if nums[1] == target
                    total += target
                end
                continue
            end
            memo = Dict{Tuple{Int, Int}, Bool}()
            if can_produce(target, nums, 2, nums[1], memo)
                total += target
            end
        end
    end
    println(total)
end

solve()
