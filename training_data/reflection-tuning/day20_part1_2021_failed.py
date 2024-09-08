def enhance_image(image, algorithm, iterations):
    def get_pixel(img, x, y, default):
        if 0 <= x < len(img) and 0 <= y < len(img[0]):
            return img[x][y]
        return default

    def enhance(img, default):
        new_img = []
        for i in range(-1, len(img) + 1):
            new_row = []
            for j in range(-1, len(img[0]) + 1):
                index = 0
                for di in [-1, 0, 1]:
                    for dj in [-1, 0, 1]:
                        index = (index << 1) | (get_pixel(img, i+di, j+dj, default) == '#')
                new_row.append(algorithm[index])
            new_img.append(new_row)
        return new_img

    current_image = image
    default_pixel = '.'

    for _ in range(iterations):
        current_image = enhance(current_image, default_pixel)
        default_pixel = algorithm[0] if default_pixel == '.' else algorithm[-1]

    return sum(row.count('#') for row in current_image)

def solve(input_data):
    algorithm, image = input_data.split('\n\n')
    algorithm = algorithm.strip()
    image = [list(line.strip()) for line in image.strip().split('\n')]

    return enhance_image(image, algorithm, 2)

# Example usage:
input_data = """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"""

result = solve(input_data)
print(result)
