
import os

def move(p, dir):
    if dir == 'N':
        return (p[0], p[1] - 1)
    elif dir == 'S':
        return (p[0], p[1] + 1)
    elif dir == 'E':
        return (p[0] + 1, p[1])
    elif dir == 'W':
        return (p[0] - 1, p[1])
    else:
        return p

def build_map(regex):
    dm = {}
    stack = []
    cp = (0, 0)
    for c in regex:
        if c == '(':
            stack.append(cp)
        elif c == '|':
            cp = stack[-1]
        elif c == ')':
            cp = stack.pop()
        else:
            np = move(cp, c)
            if cp not in dm:
                dm[cp] = {}
            dm[cp][np] = True
            cp = np
    return dm

def find_furthest_room(dm):
    visited = {}
    queue = [(0, 0)]
    max_doors = 0

    while queue:
        p = queue.pop(0)
        for np in dm.get(p, []):
            if np not in visited:
                visited[np] = visited.get(p, 0) + 1
                max_doors = max(max_doors, visited[np])
                queue.append(np)
    return max_doors

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        regex = file.read().strip()

    dm = build_map(regex[1:-1])
    max_doors = find_furthest_room(dm)
    print(max_doors)
