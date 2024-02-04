
with open('input.txt', 'r') as file:
    total_paper = 0
    for line in file:
        l, w, h = map(int, line.strip().split('x'))
        sides = [l*w, w*h, h*l]
        total_paper += 2*sum(sides) + min(sides)
    print(total_paper)
