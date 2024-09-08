def enhance_image(image, algorithm, steps):
    lit_pixels = set((i, j) for i, row in enumerate(image) for j, pixel in enumerate(row) if pixel == '#')
    min_x, min_y, max_x, max_y = 0, 0, len(image[0]) - 1, len(image) - 1
    background_lit = False

    for step in range(steps):
        new_lit_pixels = set()
        for x in range(min_x - 1, max_x + 2):
            for y in range(min_y - 1, max_y + 2):
                index = 0
                for dx in [-1, 0, 1]:
                    for dy in [-1, 0, 1]:
                        nx, ny = x + dx, y + dy
                        if (nx, ny) in lit_pixels or (background_lit and (nx < min_x or nx > max_x or ny < min_y or ny > max_y)):
                            index |= 1
                        index <<= 1
                index >>= 1
                if algorithm[index] == '#':
                    new_lit_pixels.add((x, y))
        
        lit_pixels = new_lit_pixels
        min_x, min_y = min_x - 1, min_y - 1
        max_x, max_y = max_x + 1, max_y + 1
        
        if algorithm[0] == '#' and algorithm[-1] == '.':
            background_lit = not background_lit

    return len(lit_pixels)

with open('input.txt', 'r') as file:
    algorithm = file.readline().strip()
    file.readline()  # Skip empty line
    image = [line.strip() for line in file]

print(f"Part 1: {enhance_image(image, algorithm, 2)}")
print(f"Part 2: {enhance_image(image, algorithm, 50)}")
