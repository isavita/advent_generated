
#!/bin/bash

main() {
    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    python3 -c '
import json
from functools import cmp_to_key
import sys

def read_all_stdin():
    return sys.stdin.read()

def compare(a, b):
    if isinstance(a, int) and isinstance(b, int):
        return -1 if a < b else 1 if a > b else 0
    if isinstance(a, int):
        return compare([a], b)
    if isinstance(b, int):
        return compare(a, [b])
    # Ensure a and b are lists before iterating
    if not isinstance(a, list) or not isinstance(b, list):
         # This case should ideally not be reached if inputs are always int or list
         # Handle potential errors or unexpected types if necessary
         # For simplicity, assuming valid inputs based on problem context
         pass
    for i in range(min(len(a), len(b))):
        c = compare(a[i], b[i])
        if c != 0:
            return c
    return -1 if len(a) < len(b) else 1 if len(a) > len(b) else 0

s = read_all_stdin()
packets = []
# Strip potential trailing newline before splitting pairs
for pair in s.strip().split("\n\n"):
    # Ensure pair is not empty and handle potential empty lines
    lines = [line for line in pair.split("\n") if line]
    if len(lines) == 2:
        try:
            first = json.loads(lines[0])
            second = json.loads(lines[1])
            packets.append(first)
            packets.append(second)
        except json.JSONDecodeError as e:
            print(f"Error decoding JSON in pair:\n{pair}\nError: {e}", file=sys.stderr)
            sys.exit(1)


divider1 = [[2]]
divider2 = [[6]]
packets.append(divider1)
packets.append(divider2)

# Sort using the custom comparison function
packets.sort(key=cmp_to_key(compare))

# Find 1-based indices using comparison logic for robustness
# This handles cases where simple equality check might fail due to object identity vs value
divider1_pos = -1
divider2_pos = -1
for i, packet in enumerate(packets):
    if compare(packet, divider1) == 0:
        divider1_pos = i + 1
    if compare(packet, divider2) == 0:
        divider2_pos = i + 1
    # Optimization: Stop searching once both are found
    if divider1_pos != -1 and divider2_pos != -1:
         break

if divider1_pos == -1 or divider2_pos == -1:
    print("Error: Divider packets not found after sorting.", file=sys.stderr)
    sys.exit(1)

print(divider1_pos * divider2_pos)

' < input.txt
}

main
