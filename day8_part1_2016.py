def rect(screen, a, b):
    for i in range(b):
        for j in range(a):
            screen[i][j] = 1

def rotate_row(screen, y, b):
    screen[y] = screen[y][-b:] + screen[y][:-b]

def rotate_column(screen, x, b):
    column = [screen[i][x] for i in range(len(screen))]
    column = column[-b:] + column[:-b]
    for i in range(len(screen)):
        screen[i][x] = column[i]

screen = [[0 for _ in range(50)] for _ in range(6)]

with open("input.txt") as file:
    for line in file:
        if line.startswith("rect"):
            a, b = map(int, line.split()[1].split("x"))
            rect(screen, a, b)
        elif line.startswith("rotate row"):
            y = int(line.split("=")[1].split()[0])
            b = int(line.split()[-1])
            rotate_row(screen, y, b)
        elif line.startswith("rotate column"):
            x = int(line.split("=")[1].split()[0])
            b = int(line.split()[-1])
            rotate_column(screen, x, b)

lit_pixels = sum(sum(row) for row in screen)
print(lit_pixels)