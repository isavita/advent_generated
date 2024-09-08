import re

def solve(target):
    # Extract target area coordinates
    x_min, x_max, y_min, y_max = map(int, re.findall(r'-?\d+', target))
    
    # The highest initial y velocity that still hits the target
    # is the one that makes the probe reach the bottom of the target
    # in one step after returning to y=0
    max_y_velocity = abs(y_min) - 1
    
    # The highest point is reached when the initial upward velocity becomes zero
    max_height = (max_y_velocity * (max_y_velocity + 1)) // 2
    
    return max_height

# Read input from file
with open('input.txt', 'r') as file:
    target_area = file.read().strip()

# Solve and print the result
print(solve(target_area))
