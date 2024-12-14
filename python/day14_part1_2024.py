
def solve():
    width, height = 101, 103
    robots = []
    with open("input.txt", "r") as f:
        for line in f:
            parts = line.split()
            p_part = parts[0][2:]
            v_part = parts[1][2:]
            pos = list(map(int, p_part.split(",")))
            vel = list(map(int, v_part.split(",")))
            robots.append(pos + vel)

    for _ in range(100):
        for r in robots:
            r[0] = (r[0] + r[2]) % width
            r[1] = (r[1] + r[3]) % height
            if r[0] < 0: r[0] += width
            if r[1] < 0: r[1] += height

    q1, q2, q3, q4 = 0, 0, 0, 0
    for x, y, _, _ in robots:
        if x == 50 or y == 51: continue
        if x < 50 and y < 51: q1 += 1
        elif x > 50 and y < 51: q2 += 1
        elif x < 50 and y > 51: q3 += 1
        else: q4 += 1

    print(q1 * q2 * q3 * q4)

solve()
