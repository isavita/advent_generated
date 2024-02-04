with open('input.txt', 'r') as file:
    instructions = [line.strip() for line in file]

def run_code(a):
    clock_signal = []
    idx = 0
    registers = {'a': a, 'b': 0, 'c': 0, 'd': 0}
    
    while idx < len(instructions):
        instr = instructions[idx]
        parts = instr.split()
        
        if parts[0] == 'cpy':
            if parts[1].isdigit():
                registers[parts[2]] = int(parts[1])
            else:
                registers[parts[2]] = registers[parts[1]]
        elif parts[0] == 'inc':
            registers[parts[1]] += 1
        elif parts[0] == 'dec':
            registers[parts[1]] -= 1
        elif parts[0] == 'jnz':
            val = int(parts[1]) if parts[1].isdigit() else registers[parts[1]]
            if val != 0:
                idx += int(parts[2])
                continue
        elif parts[0] == 'out':
            val = int(parts[1]) if parts[1].isdigit() else registers[parts[1]]
            clock_signal.append(val)
            if len(clock_signal) > 1 and clock_signal[-1] == clock_signal[-2]:
                return False
            if len(clock_signal) == 10:
                return True
        idx += 1

a = 0
while True:
    if run_code(a):
        break
    a += 1

print(a)