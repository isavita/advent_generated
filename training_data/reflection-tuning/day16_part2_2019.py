import numpy as np

def read_input(file_path):
    with open(file_path, 'r') as f:
        return f.read().strip()

def fft_phase(signal):
    n = len(signal)
    output = np.zeros_like(signal)
    for i in range(n):
        pattern = np.repeat([0, 1, 0, -1], i + 1)
        pattern = np.tile(pattern, n // len(pattern) + 1)[:n]
        pattern = np.roll(pattern, -1)
        output[i] = abs(np.sum(signal * pattern)) % 10
    return output

def part1(signal, phases=100):
    for _ in range(phases):
        signal = fft_phase(signal)
    return ''.join(map(str, signal[:8]))

def part2(signal, repeat=10000, phases=100):
    offset = int(''.join(map(str, signal[:7])))
    signal = np.tile(signal, repeat)[offset:]
    
    for _ in range(phases):
        signal = np.cumsum(signal[::-1]) % 10
        signal = signal[::-1]
    
    return ''.join(map(str, signal[:8]))

def main():
    input_signal = np.array([int(d) for d in read_input('input.txt')])
    
    print("Part 1:", part1(input_signal))
    print("Part 2:", part2(input_signal))

if __name__ == "__main__":
    main()
