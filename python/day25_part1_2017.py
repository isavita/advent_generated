def read_blueprint(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f if line.strip()]

    # Extracting initial state and steps
    initial_state = lines[0].split()[-1].rstrip('.')
    steps = int(lines[1].split()[-2])
    
    # Parsing state instructions
    states = {}
    i = 2  # Start reading states from the 3rd line onwards
    while i < len(lines):
        state_name = lines[i][-2]  # The second last character is the state name
        i += 1  # Move to the instructions for this state
        
        states[state_name] = {}
        for _ in range(2):  # Two sets of instructions per state
            val = int(lines[i][-2])  # Current value condition is the second last character
            write_val = int(lines[i + 1].split()[-1][0])
            move_dir = -1 if "left" in lines[i + 2] else 1
            next_state = lines[i + 3].split()[-1].rstrip('.')
            states[state_name][val] = (write_val, move_dir, next_state)
            i += 4  # Move to next set of instructions or next state
        
    return initial_state, steps, states

def run_turing_machine(initial_state, steps, states):
    tape = {}
    cursor = 0
    state = initial_state
    
    for _ in range(steps):
        current_val = tape.get(cursor, 0)
        write_val, move_dir, next_state = states[state][current_val]
        tape[cursor] = write_val
        cursor += move_dir
        state = next_state
    
    return sum(tape.values())

def main():
    initial_state, steps, states = read_blueprint('input.txt')
    checksum = run_turing_machine(initial_state, steps, states)
    print(checksum)

if __name__ == "__main__":
    main()
