with open("input.txt", "r") as file:
    total_code = 0
    total_memory = 0
    total_encoded = 0
    
    for line in file:
        line = line.strip()
        total_code += len(line)
        
        memory = eval(line)
        total_memory += len(memory)
        
        encoded = "\"" + line.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
        total_encoded += len(encoded)
    
    print(total_code - total_memory)
    print(total_encoded - total_code)