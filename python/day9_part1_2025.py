
import re
import sys

def main() -> None:
    try:
        txt = open("input.txt").read()
    except FileNotFoundError:
        sys.stderr.write("input.txt not found\n")
        return

    nums = list(map(int, re.findall(r"-?\d+", txt)))
    xs = nums[0::2]
    ys = nums[1::2]
    n = len(xs)

    best = 0
    for i in range(n):
        xi, yi = xs[i], ys[i]
        for j in range(i, n):
            dx = abs(xi - xs[j]) + 1
            dy = abs(yi - ys[j]) + 1
            area = dx * dy
            if area > best:
                best = area

    print(best)


if __name__ == "__main__":
    main()
