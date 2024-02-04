
import math

def find_steps(num):
    root = math.ceil(math.sqrt(num))
    if root % 2 == 0:
        root += 1
    side_length = root - 1
    steps_to_center = side_length // 2
    max_num = root ** 2
    steps_to_max_num = max_num - num
    steps_along_side = steps_to_max_num % side_length
    return steps_to_center + abs(steps_along_side - steps_to_center)

with open("input.txt", "r") as file:
    num = int(file.read().strip())
    print(find_steps(num))
