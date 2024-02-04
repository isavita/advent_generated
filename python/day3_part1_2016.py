with open('input.txt', 'r') as file:
    triangles = [list(map(int, line.split())) for line in file.readlines()]

possible_triangles = 0

for triangle in triangles:
    sorted_triangle = sorted(triangle)
    if sorted_triangle[0] + sorted_triangle[1] > sorted_triangle[2]:
        possible_triangles += 1

print(possible_triangles)