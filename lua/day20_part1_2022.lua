
local function mix(nums)
    local n = #nums - 1
    for i = 1, #nums do
        local oldpos = nums[i].pos
        local newpos = ((oldpos + nums[i].val) % n + n) % n
        if oldpos < newpos then
            for j = 1, #nums do
                if nums[j].pos > oldpos and nums[j].pos <= newpos then
                    nums[j].pos = nums[j].pos - 1
                end
            end
        elseif newpos < oldpos then
            for j = 1, #nums do
                if nums[j].pos >= newpos and nums[j].pos < oldpos then
                    nums[j].pos = nums[j].pos + 1
                end
            end
        end
        nums[i].pos = newpos
    end
end

local function coords(nums)
    local l = #nums
    local zeroPos = 0
    for i = 1, l do
        if nums[i].val == 0 then
            zeroPos = nums[i].pos
            break
        end
    end
    local sum_val = 0
    for i = 1, l do
        local p = nums[i].pos
        if p == (zeroPos + 1000) % l or p == (zeroPos + 2000) % l or p == (zeroPos + 3000) % l then
            sum_val = sum_val + nums[i].val
        end
    end
    return sum_val
end

local function main()
    local nums = {}
    local i = 1
    for line in io.lines("input.txt") do
        nums[i] = {pos = i - 1, val = tonumber(line)}
        i = i + 1
    end

    mix(nums)
    print(coords(nums))

end

main()
