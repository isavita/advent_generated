
with open('input.txt', 'r') as file:
    depths = [int(line.strip()) for line in file]

increased_measurements = sum(1 for i in range(1, len(depths)) if depths[i] > depths[i-1])
print(increased_measurements)

sliding_sums = [sum(depths[i:i+3]) for i in range(len(depths)-2)]
increased_sums = sum(1 for i in range(1, len(sliding_sums)) if sliding_sums[i] > sliding_sums[i-1])
print(increased_sums)
