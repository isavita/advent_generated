
import numpy as np

lights = np.zeros((1000, 1000), dtype=int)
with open('input.txt') as f:
    for line in f:
        words = line.split()
        if words[0] == 'turn':
            start = list(map(int, words[2].split(',')))
            end = list(map(int, words[4].split(',')))
            if words[1] == 'on':
                lights[start[0]:end[0]+1, start[1]:end[1]+1] += 1
            elif words[1] == 'off':
                lights[start[0]:end[0]+1, start[1]:end[1]+1] -= 1
                lights[lights < 0] = 0
        elif words[0] == 'toggle':
            start = list(map(int, words[1].split(',')))
            end = list(map(int, words[3].split(',')))
            lights[start[0]:end[0]+1, start[1]:end[1]+1] += 2

print(np.sum(lights))
