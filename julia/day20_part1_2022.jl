
struct Num
    pos::Int
    val::Int
end

function mix(nums)
    n = length(nums) - 1
    for i in eachindex(nums)
        oldpos = nums[i].pos
        newpos = ((oldpos + nums[i].val) % n + n) % n
        if oldpos < newpos
            for j in eachindex(nums)
                if nums[j].pos > oldpos && nums[j].pos <= newpos
                    nums[j] = Num(nums[j].pos - 1, nums[j].val)
                end
            end
        end
        if newpos < oldpos
            for j in eachindex(nums)
                if nums[j].pos >= newpos && nums[j].pos < oldpos
                    nums[j] = Num(nums[j].pos + 1, nums[j].val)
                end
            end
        end
        nums[i] = Num(newpos, nums[i].val)
    end
end

function coords(nums)
    l = length(nums)
    zeroPos = 0
    for i in eachindex(nums)
        if nums[i].val == 0
            zeroPos = nums[i].pos
            break
        end
    end
    sum = 0
    for i in eachindex(nums)
        if nums[i].pos == (zeroPos + 1000) % l || nums[i].pos == (zeroPos + 2000) % l || nums[i].pos == (zeroPos + 3000) % l
            sum += nums[i].val
        end
    end
    return sum
end

function main()
    nums = Num[]
    for (i, n) in enumerate(eachline("input.txt"))
        push!(nums, Num(i, parse(Int, n)))
    end
    nums2 = [Num(num.pos, 811589153 * num.val) for num in nums]
    mix(nums)
    println(coords(nums))
end

main()
