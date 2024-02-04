with open('input.txt') as f:
    instructions = [line.strip() for line in f]

def run_program(instructions):
    acc = 0
    idx = 0
    visited = set()
    
    while idx not in visited and idx < len(instructions):
        visited.add(idx)
        op, arg = instructions[idx].split()
        
        if op == 'acc':
            acc += int(arg)
            idx += 1
        elif op == 'jmp':
            idx += int(arg)
        else:
            idx += 1
    
    return acc, idx == len(instructions)

def fix_program(instructions):
    for i in range(len(instructions)):
        op, arg = instructions[i].split()
        if op == 'jmp':
            new_instructions = instructions[:i] + ['nop ' + arg] + instructions[i+1:]
        elif op == 'nop':
            new_instructions = instructions[:i] + ['jmp ' + arg] + instructions[i+1:]
        else:
            continue
        
        acc, terminated = run_program(new_instructions)
        if terminated:
            return acc

print(fix_program(instructions))