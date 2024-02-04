
import sys

class Num:
    def __init__(self, pos, val):
        self.pos = pos
        self.val = val

def mix(nums):
    n = len(nums) - 1
    for i in range(len(nums)):
        oldpos = nums[i].pos
        newpos = ((oldpos + nums[i].val) % n + n) % n
        if oldpos < newpos:
            for j in range(len(nums)):
                if nums[j].pos > oldpos and nums[j].pos <= newpos:
                    nums[j].pos -= 1
        if newpos < oldpos:
            for j in range(len(nums)):
                if nums[j].pos >= newpos and nums[j].pos < oldpos:
                    nums[j].pos += 1
        nums[i].pos = newpos

def coords(nums):
    l = len(nums)
    zeroPos = 0
    for i in range(len(nums)):
        if nums[i].val == 0:
            zeroPos = nums[i].pos
            break
    sum_val = 0
    for i in range(len(nums)):
        if nums[i].pos == (zeroPos + 1000) % l or nums[i].pos == (zeroPos + 2000) % l or nums[i].pos == (zeroPos + 3000) % l:
            sum_val += nums[i].val
    return sum_val

def main():
    nums = []
    with open("input.txt", "r") as file:
        lines = file.readlines()
        for i, n in enumerate(lines):
            nums.append(Num(i, int(n.strip())))
    
    nums2 = [Num(num.pos, 811589153 * num.val) for num in nums]

    mix(nums)
    print(coords(nums))

if __name__ == "__main__":
    main()
