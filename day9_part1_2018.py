
from collections import deque

with open("input.txt") as f:
    data = f.readline().split()
    players = int(data[0])
    last_marble = int(data[6])

def play_marble_game(players, last_marble):
    scores = [0] * players
    circle = deque([0])

    for i in range(1, last_marble + 1):
        if i % 23 == 0:
            circle.rotate(7)
            scores[i % players] += i + circle.pop()
            circle.rotate(-1)
        else:
            circle.rotate(-1)
            circle.append(i)

    return max(scores)

print(play_marble_game(players, last_marble))
