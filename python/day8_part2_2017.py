input_file = open("input.txt", "r")
instructions = input_file.read().strip().split('\n')
input_file.close()

registers = {}
max_value = 0

for instruction in instructions:
    reg, op, val, _, cond_reg, cond_op, cond_val = instruction.split()
    val = int(val)
    cond_val = int(cond_val)
    
    if reg not in registers:
        registers[reg] = 0
    if cond_reg not in registers:
        registers[cond_reg] = 0
    
    if (cond_op == "==" and registers[cond_reg] == cond_val) or \
       (cond_op == "!=" and registers[cond_reg] != cond_val) or \
       (cond_op == ">" and registers[cond_reg] > cond_val) or \
       (cond_op == "<" and registers[cond_reg] < cond_val) or \
       (cond_op == ">=" and registers[cond_reg] >= cond_val) or \
       (cond_op == "<=" and registers[cond_reg] <= cond_val):
        
        if op == "inc":
            registers[reg] += val
        elif op == "dec":
            registers[reg] -= val
        
        max_value = max(max_value, registers[reg])

print(max(registers.values()))
print(max_value)