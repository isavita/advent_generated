
lines = [line.strip() for line in open("input.txt")]

tree_count = 0
x, y = 0, 0
while y < len(lines):
    if lines[y][x % len(lines[0])] == "#":
        tree_count += 1
    x += 3
    y += 1

print(tree_count)
