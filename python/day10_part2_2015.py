with open('input.txt', 'r') as file:
    sequence = file.read().strip()

for _ in range(40):
    new_sequence = ''
    i = 0
    while i < len(sequence):
        count = 1
        while i + 1 < len(sequence) and sequence[i] == sequence[i + 1]:
            i += 1
            count += 1
        new_sequence += str(count) + sequence[i]
        i += 1
    sequence = new_sequence

print(len(sequence))

for _ in range(10):
    new_sequence = ''
    i = 0
    while i < len(sequence):
        count = 1
        while i + 1 < len(sequence) and sequence[i] == sequence[i + 1]:
            i += 1
            count += 1
        new_sequence += str(count) + sequence[i]
        i += 1
    sequence = new_sequence

print(len(sequence))