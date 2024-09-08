def parse_value(registers, value):
    return registers[value] if value.isalpha() else int(value)

def duet(instructions):
    registers = {chr(i): 0 for i in range(ord('a'), ord('z')+1)}
    last_sound = None
    i = 0

    while 0 <= i < len(instructions):
        cmd, *args = instructions[i].split()

        if cmd == 'snd':
            last_sound = parse_value(registers, args[0])
        elif cmd == 'set':
            registers[args[0]] = parse_value(registers, args[1])
        elif cmd == 'add':
            registers[args[0]] += parse_value(registers, args[1])
        elif cmd == 'mul':
            registers[args[0]] *= parse_value(registers, args[1])
        elif cmd == 'mod':
            registers[args[0]] %= parse_value(registers, args[1])
        elif cmd == 'rcv':
            if parse_value(registers, args[0]) != 0:
                return last_sound
        elif cmd == 'jgz':
            if parse_value(registers, args[0]) > 0:
                i += parse_value(registers, args[1]) - 1

        i += 1

    return None

# Example usage
instructions = [
    "set a 1",
    "add a 2",
    "mul a a",
    "mod a 5",
    "snd a",
    "set a 0",
    "rcv a",
    "jgz a -1",
    "set a 1",
    "jgz a -2"
]

result = duet(instructions)
print(f"The recovered frequency is: {result}")
