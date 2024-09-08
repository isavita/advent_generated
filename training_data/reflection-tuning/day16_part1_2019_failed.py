import numpy as np

def generate_pattern(length, position):
    base_pattern = [0, 1, 0, -1]
    pattern = np.repeat(base_pattern, position)
    pattern = np.tile(pattern, length // len(pattern) + 1)
    return pattern[1:length+1]

def fft_phase(signal):
    length = len(signal)
    output = np.zeros(length, dtype=int)
    for i in range(length):
        pattern = generate_pattern(length, i + 1)
        output[i] = abs(np.sum(signal * pattern)) % 10
    return output

def fft(signal, phases):
    for _ in range(phases):
        signal = fft_phase(signal)
    return signal

def solve(input_signal):
    signal = np.array([int(d) for d in input_signal])
    result = fft(signal, 100)
    return ''.join(map(str, result[:8]))

# Example usage:
input_signal = "80871224585914546619083218645595"
print(solve(input_signal))
