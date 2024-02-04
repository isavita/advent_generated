
from functools import reduce

def knot_hash(lengths, rounds=64):
    lst = list(range(256))
    pos = 0
    skip = 0
    
    lengths = list(map(ord, lengths)) + [17, 31, 73, 47, 23]
    
    for _ in range(rounds):
        for length in lengths:
            lst = lst[pos:] + lst[:pos]
            lst = lst[:length][::-1] + lst[length:]
            lst = lst[-pos:] + lst[:-pos]
            pos = (pos + length + skip) % 256
            skip += 1
    
    dense_hash = [reduce(lambda x, y: x ^ y, lst[i:i+16]) for i in range(0, 256, 16)]
    
    return ''.join(format(num, '02x') for num in dense_hash)

with open('input.txt', 'r') as file:
    puzzle_input = file.read().strip()

print(knot_hash(puzzle_input))
