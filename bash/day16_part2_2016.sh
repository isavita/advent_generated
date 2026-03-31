
#!/bin/bash
python3 -c "
s = open('input.txt').read().strip()
L = 35651584
t = str.maketrans('01', '10')
while len(s) < L:
    s += '0' + s[::-1].translate(t)
s = s[:L]
while len(s) % 2 == 0:
    s = ''.join('1' if a == b else '0' for a, b in zip(s[::2], s[1::2]))
print(f'Checksum: {s}')
"
