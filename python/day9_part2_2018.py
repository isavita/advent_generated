
from collections import deque

with open('input.txt') as f:
    data = f.readline().strip().split()

players = int(data[0])
last_marble = int(data[6]) * 100

circle = deque([0])
scores = [0] * players
current_player = 0

for marble in range(1, last_marble + 1):
    if marble % 23 == 0:
        circle.rotate(7)
        scores[current_player] += marble + circle.pop()
        circle.rotate(-1)
    else:
        circle.rotate(-1)
        circle.append(marble)

    current_player = (current_player + 1) % players

print(max(scores))
