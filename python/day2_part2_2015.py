
total_paper = 0
total_ribbon = 0

with open("input.txt") as f:
    for line in f:
        l, w, h = map(int, line.strip().split('x'))
        sides = [l*w, w*h, h*l]
        total_paper += 2*sum(sides) + min(sides)
        total_ribbon += 2*min(l+w, w+h, h+l) + l*w*h

print(total_paper)
print(total_ribbon)
