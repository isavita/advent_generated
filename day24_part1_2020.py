input_file = open("input.txt", "r")
instructions = input_file.read().splitlines()

black_tiles = set()

for instruction in instructions:
    x, y = 0, 0
    i = 0
    while i < len(instruction):
        if instruction[i] == "e":
            x += 1
            i += 1
        elif instruction[i] == "w":
            x -= 1
            i += 1
        elif instruction[i:i+2] == "se":
            y -= 1
            i += 2
        elif instruction[i:i+2] == "sw":
            x -= 1
            y -= 1
            i += 2
        elif instruction[i:i+2] == "ne":
            x += 1
            y += 1
            i += 2
        elif instruction[i:i+2] == "nw":
            y += 1
            i += 2
    
    if (x, y) in black_tiles:
        black_tiles.remove((x, y))
    else:
        black_tiles.add((x, y))

print(len(black_tiles))