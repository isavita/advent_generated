
import sys

with open("input.txt", "r") as file:
    program = file.readline().strip().split(",")

memory = {}
for i, s in enumerate(program):
    memory[i] = int(s)

output = 0
ip = 0
relativeBase = 0

while True:
    opcode = memory[ip] % 100
    modes = str(memory[ip] // 100)

    def getParam(offset):
        mode = 0
        if len(modes) >= offset:
            mode = int(modes[-offset])
        
        param = memory[ip + offset]
        if mode == 0:
            return memory[param]
        elif mode == 1:
            return param
        elif mode == 2:
            return memory[relativeBase + param]
        else:
            raise ValueError("unknown parameter mode")

    def setParam(offset, value):
        mode = 0
        if len(modes) >= offset:
            mode = int(modes[-offset])
        
        param = memory[ip + offset]
        if mode == 0:
            memory[param] = value
        elif mode == 2:
            memory[relativeBase + param] = value
        else:
            raise ValueError("unknown parameter mode")

    if opcode == 1:
        setParam(3, getParam(1) + getParam(2))
        ip += 4
    elif opcode == 2:
        setParam(3, getParam(1) * getParam(2))
        ip += 4
    elif opcode == 3:
        setParam(1, 1)  # Test mode input
        ip += 2
    elif opcode == 4:
        output = getParam(1)
        ip += 2
    elif opcode == 5:
        if getParam(1) != 0:
            ip = getParam(2)
        else:
            ip += 3
    elif opcode == 6:
        if getParam(1) == 0:
            ip = getParam(2)
        else:
            ip += 3
    elif opcode == 7:
        if getParam(1) < getParam(2):
            setParam(3, 1)
        else:
            setParam(3, 0)
        ip += 4
    elif opcode == 8:
        if getParam(1) == getParam(2):
            setParam(3, 1)
        else:
            setParam(3, 0)
        ip += 4
    elif opcode == 9:
        relativeBase += getParam(1)
        ip += 2
    elif opcode == 99:
        print(output)
        break
    else:
        raise ValueError(f"unknown opcode: {opcode}")
