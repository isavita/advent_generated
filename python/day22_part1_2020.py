
p1, p2 = open('input.txt').read().split('\n\n')
p1, p2 = map(lambda x: list(map(int, x.split('\n')[1:])), [p1, p2])

while p1 and p2:
    c1, c2 = p1.pop(0), p2.pop(0)
    if c1 > c2:
        p1.extend([c1, c2])
    else:
        p2.extend([c2, c1])

winner = p1 if p1 else p2
score = sum((i + 1) * card for i, card in enumerate(winner[::-1]))
print(score)
