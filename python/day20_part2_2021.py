
def read_input(filename):
    with open(filename, 'r') as file:
        algorithm = file.readline().strip()

        # skip the empty line
        file.readline()

        image = []
        for line in file:
            row = [char == '#' for char in line.strip()]
            image.append(row)

    return algorithm, image

def enhance_image(algorithm, image, use_infinite_lit):
    expand_by = 1
    new_image = [[False for _ in range(len(image[0]) + (expand_by*2))] for _ in range(len(image) + (expand_by*2))]

    for y in range(-expand_by, len(image) + expand_by):
        for x in range(-expand_by, len(image[0]) + expand_by):
            index = 0
            for dy in range(-1, 2):
                for dx in range(-1, 2):
                    index <<= 1
                    ny, nx = y + dy, x + dx
                    if 0 <= ny < len(image) and 0 <= nx < len(image[0]):
                        if image[ny][nx]:
                            index |= 1
                    elif use_infinite_lit:
                        index |= 1
            new_image[y + expand_by][x + expand_by] = algorithm[index] == '#'

    return new_image

def count_lit_pixels(image):
    return sum(1 for row in image for pixel in row if pixel)

if __name__ == '__main__':
    iterations = 50

    algorithm, image = read_input("input.txt")
    for i in range(iterations):
        image = enhance_image(algorithm, image, i % 2 == 1 and algorithm[0] == '#')

    print(count_lit_pixels(image))
