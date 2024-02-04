
import sys

def read_input(filename):
    with open(filename, 'r') as file:
        algorithm = file.readline().strip()
        file.readline()  # Skip the empty line
        image = [list(line.strip()) for line in file]
    return algorithm, image

def enhance_image(image, algorithm, times):
    for i in range(times):
        image = apply_algorithm(image, algorithm, i % 2 == 1 and algorithm[0] == '#')
    return image

def apply_algorithm(image, algorithm, flip):
    enhanced_image = [['' for _ in range(len(image[0]) + 2)] for _ in range(len(image) + 2)]
    for i in range(len(enhanced_image)):
        for j in range(len(enhanced_image[i])):
            index = calculate_index(i - 1, j - 1, image, flip)
            enhanced_image[i][j] = algorithm[index]
    return enhanced_image

def calculate_index(i, j, image, flip):
    index = 0
    for di in range(-1, 2):
        for dj in range(-1, 2):
            index <<= 1
            if 0 <= i + di < len(image) and 0 <= j + dj < len(image[0]):
                if image[i + di][j + dj] == '#':
                    index |= 1
            elif flip:
                index |= 1
    return index

def count_lit_pixels(image):
    count = sum(row.count('#') for row in image)
    return count

if __name__ == "__main__":
    algorithm, image = read_input("input.txt")
    image = enhance_image(image, algorithm, 2)
    print(count_lit_pixels(image))
