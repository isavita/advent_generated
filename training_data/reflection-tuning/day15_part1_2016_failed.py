def parse_input(input_data):
    discs = []
    for line in input_data.split('\n'):
        parts = line.split()
        positions = int(parts[3])
        start_pos = int(parts[-1][:-1])
        discs.append((positions, start_pos))
    return discs

def find_button_press_time(discs):
    time = 0
    while True:
        if all((start_pos + time + i + 1) % positions == 0 
               for i, (positions, start_pos) in enumerate(discs)):
            return time
        time += 1

# Example input
input_data = """Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1."""

discs = parse_input(input_data)
result = find_button_press_time(discs)
print(result)
