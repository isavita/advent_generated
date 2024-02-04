
import math

def main():
    cubes = {}
    neighbors = [
        (-1, 0, 0),
        (1, 0, 0),
        (0, -1, 0),
        (0, 1, 0),
        (0, 0, -1),
        (0, 0, 1)
    ]
    min_val = (math.inf, math.inf, math.inf)
    max_val = (-math.inf, -math.inf, -math.inf)

    with open("input.txt", "r") as file:
        for line in file:
            if line.strip() == "":
                continue
            cube = tuple(map(int, line.strip().split(",")))
            cubes[cube] = None
            min_val = tuple(min(min_val[i], cube[i]) for i in range(3))
            max_val = tuple(max(max_val[i], cube[i]) for i in range(3))
    
    min_val = tuple(val - 1 for val in min_val)
    max_val = tuple(val + 1 for val in max_val)

    faces = 0
    q = [min_val]
    seen = {min_val}

    while q:
        curr = q.pop(0)
        for delta in neighbors:
            next_pos = tuple(curr[i] + delta[i] for i in range(3))
            if any(next_pos[i] < min_val[i] or next_pos[i] > max_val[i] for i in range(3)):
                continue
            if next_pos in cubes:
                faces += 1
            elif next_pos not in seen:
                seen.add(next_pos)
                q.append(next_pos)
    
    print(faces)

if __name__ == "__main__":
    main()
