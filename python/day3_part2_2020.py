
with open('input.txt') as f:
    data = [line.strip() for line in f]

def count_trees(right, down):
    trees = 0
    col = 0
    for row in range(0, len(data), down):
        if data[row][col % len(data[row])] == '#':
            trees += 1
        col += right
    return trees

result = count_trees(1, 1) * count_trees(3, 1) * count_trees(5, 1) * count_trees(7, 1) * count_trees(1, 2)
print(result)
