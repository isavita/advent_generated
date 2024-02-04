with open('input.txt', 'r') as file:
    total_code = 0
    total_memory = 0
    for line in file:
        line = line.strip()
        total_code += len(line)
        total_memory += len(eval(line))
    
print(total_code - total_memory)