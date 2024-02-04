
with open('input.txt') as f:
    data = f.read().strip().split('\n')

firewall = {int(x.split(':')[0]): int(x.split(':')[1]) for x in data}

def caught(delay):
    return any((delay + depth) % (2 * (range - 1)) == 0 for depth, range in firewall.items())

delay = 0
while caught(delay):
    delay += 1

print(delay)
