def hash_algorithm(s):
    current = 0
    for char in s:
        current = ((current + ord(char)) * 17) % 256
    return current

def process_sequence(sequence):
    boxes = [[] for _ in range(256)]
    
    for step in sequence.split(','):
        if '=' in step:
            label, focal_length = step.split('=')
            box = hash_algorithm(label)
            focal_length = int(focal_length)
            
            for i, (existing_label, _) in enumerate(boxes[box]):
                if existing_label == label:
                    boxes[box][i] = (label, focal_length)
                    break
            else:
                boxes[box].append((label, focal_length))
        
        elif '-' in step:
            label = step[:-1]
            box = hash_algorithm(label)
            boxes[box] = [(l, f) for l, f in boxes[box] if l != label]

    return boxes

def calculate_focusing_power(boxes):
    total_power = 0
    for box_num, box in enumerate(boxes):
        for slot, (_, focal_length) in enumerate(box, 1):
            total_power += (box_num + 1) * slot * focal_length
    return total_power

# Read input from file
with open('input.txt', 'r') as file:
    sequence = file.read().strip()

# Process the sequence and calculate focusing power
boxes = process_sequence(sequence)
focusing_power = calculate_focusing_power(boxes)

print(focusing_power)
