
import os

def read_input():
    with open("input.txt", "r") as file:
        return file.read().strip().split("\n")

def mix(nums):
    n = len(nums) - 1
    for i in range(len(nums)):
        oldpos = nums[i][0]
        newpos = ((oldpos + nums[i][1]) % n + n) % n
        if oldpos < newpos:
            for j in range(len(nums)):
                if nums[j][0] > oldpos and nums[j][0] <= newpos:
                    nums[j][0] -= 1
        if newpos < oldpos:
            for j in range(len(nums)):
                if nums[j][0] >= newpos and nums[j][0] < oldpos:
                    nums[j][0] += 1
        nums[i][0] = newpos

def coords(nums):
    l = len(nums)
    zero_pos = 0
    for i in range(len(nums)):
        if nums[i][1] == 0:
            zero_pos = nums[i][0]
            break
    total_sum = 0
    for i in range(len(nums)):
        if nums[i][0] == (zero_pos + 1000) % l or nums[i][0] == (zero_pos + 2000) % l or nums[i][0] == (zero_pos + 3000) % l:
            total_sum += nums[i][1]
    return total_sum

nums = [[i, int(n)] for i, n in enumerate(read_input())]
nums2 = [[nums[i][0], 811589153 * nums[i][1]] for i in range(len(nums))]

for i in range(10):
    mix(nums2)
print(coords(nums2))
