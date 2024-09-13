def read_input():
    with open('input.txt', 'r') as f:
        return [line.strip() for line in f]

def is_register(x):
    return x in 'abcd'

def get_value(x, registers):
    if is_register(x):
        return registers[x]
    else:
        return int(x)

def execute_program(instructions, registers):
    i = 0
    while i < len(instructions):
        # Optimization: Check for specific patterns in instructions
        # that can be optimized. For example, loops that perform multiplication.

        # Check for multiplication pattern:
        # This pattern represents a nested loop that can be replaced with multiplication.
        if (i + 5 < len(instructions)):
            pattern = instructions[i:i+6]
            if (pattern[0].startswith('cpy') and
                pattern[1].startswith('inc') and
                pattern[2].startswith('dec') and
                pattern[3].startswith('jnz') and
                pattern[4].startswith('dec') and
                pattern[5].startswith('jnz')):
                
                # Extract variables
                cpy_x, cpy_y = pattern[0].split()[1:]
                inc_a = pattern[1].split()[1]
                dec_c = pattern[2].split()[1]
                jnz_c, jnz_c_offset = pattern[3].split()[1:]
                dec_d = pattern[4].split()[1]
                jnz_d, jnz_d_offset = pattern[5].split()[1:]

                if (inc_a == 'a' and dec_c == cpy_y and jnz_c == cpy_y and int(jnz_c_offset) == -2 and
                    dec_d == 'd' and jnz_d == 'd' and int(jnz_d_offset) == -5):
                    # Perform optimized multiplication
                    registers['a'] += registers[cpy_x] * registers['d']
                    registers[cpy_y] = 0
                    registers['d'] = 0
                    i += 6
                    continue

        # Handle toggling and normal execution
        parts = instructions[i].split()
        cmd = parts[0]
        
        # Handle toggling
        if cmd == 'tgl':
            x = get_value(parts[1], registers)
            target_idx = i + x
            if 0 <= target_idx < len(instructions):
                target_parts = instructions[target_idx].split()
                if len(target_parts) == 2:
                    if target_parts[0] == 'inc':
                        target_parts[0] = 'dec'
                    else:
                        target_parts[0] = 'inc'
                elif len(target_parts) == 3:
                    if target_parts[0] == 'jnz':
                        target_parts[0] = 'cpy'
                    else:
                        target_parts[0] = 'jnz'
                instructions[target_idx] = ' '.join(target_parts)
            i += 1
            continue
        
        if cmd == 'cpy':
            x, y = parts[1], parts[2]
            if is_register(y):
                registers[y] = get_value(x, registers)
            i += 1
        elif cmd == 'inc':
            x = parts[1]
            if is_register(x):
                registers[x] += 1
            i += 1
        elif cmd == 'dec':
            x = parts[1]
            if is_register(x):
                registers[x] -= 1
            i += 1
        elif cmd == 'jnz':
            x, y = parts[1], parts[2]
            if get_value(x, registers) != 0:
                i += get_value(y, registers)
            else:
                i += 1
        else:
            # Invalid instruction, skip
            i += 1

def main():
    instructions = read_input()
    registers = {'a': 12, 'b': 0, 'c': 0, 'd': 0}
    execute_program(instructions, registers)
    print(registers['a'])

if __name__ == '__main__':
    main()
