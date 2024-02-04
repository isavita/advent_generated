with open("input.txt", "r") as file:
    instructions = [line.strip().split(" -> ") for line in file.readlines()]

wires = {}

def get_signal(wire):
    if wire.isdigit():
        return int(wire)
    if wire in wires:
        return wires[wire]
    for instr in instructions:
        if instr[-1] == wire:
            if len(instr[0].split()) == 1:
                signal = get_signal(instr[0])
            elif "AND" in instr[0]:
                signal = get_signal(instr[0].split()[0]) & get_signal(instr[0].split()[2])
            elif "OR" in instr[0]:
                signal = get_signal(instr[0].split()[0]) | get_signal(instr[0].split()[2])
            elif "LSHIFT" in instr[0]:
                signal = get_signal(instr[0].split()[0]) << int(instr[0].split()[2])
            elif "RSHIFT" in instr[0]:
                signal = get_signal(instr[0].split()[0]) >> int(instr[0].split()[2])
            elif "NOT" in instr[0]:
                signal = ~get_signal(instr[0].split()[1]) & 0xffff
            wires[wire] = signal
            return signal

answer = get_signal("a")
print(answer)