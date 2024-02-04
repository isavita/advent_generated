
import re

total_power = 0

with open("input.txt", "r") as file:
    for line in file:
        matches = re.match(r'Game (\d+): (.+)', line)
        
        if matches:
            rounds = matches.group(2).split(";")
            max_red, max_green, max_blue = 0, 0, 0
            
            for round in rounds:
                cubes = re.findall(r'(\d+) (red|green|blue)', round)
                red, green, blue = 0, 0, 0
                
                for cube in cubes:
                    count = int(cube[0])
                    color = cube[1]
                    
                    if color == "red":
                        red += count
                    elif color == "green":
                        green += count
                    elif color == "blue":
                        blue += count
                
                max_red = max(max_red, red)
                max_green = max(max_green, green)
                max_blue = max(max_blue, blue)
            
            power = max_red * max_green * max_blue
            total_power += power

print(total_power)
