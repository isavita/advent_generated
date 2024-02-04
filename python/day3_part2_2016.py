import numpy as np

triangles = np.loadtxt("input.txt")

possible_triangles = 0
for i in range(0, len(triangles), 3):
    for j in range(3):
        sides = sorted([triangles[i][j], triangles[i+1][j], triangles[i+2][j]])
        if sides[0] + sides[1] > sides[2]:
            possible_triangles += 1

print(possible_triangles)