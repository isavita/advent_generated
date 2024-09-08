from collections import defaultdict

def parse_input(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    
    start_state = lines[0].split()[-1][0]
    steps = int(lines[1].split()[-2])
    
    states = {}
    for i in range(3, len(lines), 10):
        state = lines[i].split()[-1][0]
        states[state] = {
            0: (int(lines[i+2].split()[-1][0]), 1 if lines[i+3].split()[-1] == 'right.' else -1, lines[i+4].split()[-1][0]),
            1: (int(lines[i+6].split()[-1][0]), 1 if lines[i+7].split()[-1] == 'right.' else -1, lines[i+8].split()[-1][0])
        }
    
    return start_state, steps, states

def run_turing_machine(start_state, steps, states):
    tape = defaultdict(int)
    cursor = 0
    current_state = start_state

    for _ in range(steps):
        value, move, next_state = states[current_state][tape[cursor]]
        tape[cursor] = value
        cursor += move
        current_state = next_state

    return sum(tape.values())

def main():
    start_state, steps, states = parse_input('input.txt')
    checksum = run_turing_machine(start_state, steps, states)
    print(f"The diagnostic checksum is: {checksum}")

if __name__ == "__main__":
    main()
