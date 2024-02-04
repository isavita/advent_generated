
with open("input.txt", "r") as file:
    lines = file.readlines()

head = [0, 0]
tail = [0, 0]
visited = {(0, 0)}

for line in lines:
    dir, steps = line.strip().split()
    numSteps = int(steps)

    for i in range(numSteps):
        if dir == "R":
            head[0] += 1
        elif dir == "L":
            head[0] -= 1
        elif dir == "U":
            head[1] += 1
        elif dir == "D":
            head[1] -= 1

        if abs(head[0] - tail[0]) > 1 or abs(head[1] - tail[1]) > 1:
            if head[0] != tail[0] and head[1] != tail[1]:
                tail[0] += 1 if head[0] > tail[0] else -1
                tail[1] += 1 if head[1] > tail[1] else -1
            else:
                tail[0] += 1 if head[0] > tail[0] else -1 if head[0] < tail[0] else 0
                tail[1] += 1 if head[1] > tail[1] else -1 if head[1] < tail[1] else 0

        visited.add(tuple(tail))

print(len(visited))
