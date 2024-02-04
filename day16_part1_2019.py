
from itertools import cycle

def fft(signal):
    base_pattern = [0, 1, 0, -1]
    output = [0] * len(signal)
    
    for i in range(len(signal)):
        pattern = cycle([elem for elem in base_pattern for _ in range(i + 1)])
        next(pattern)  # skip the first value
        output[i] = abs(sum(int(digit) * next(pattern) for digit in signal)) % 10
    
    return output

with open("input.txt", "r") as file:
    signal = file.read().strip()

for _ in range(100):
    signal = fft(signal)

print("".join(map(str, signal[:8])))
