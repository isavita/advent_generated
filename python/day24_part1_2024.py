
import re

def solve():
    with open("input.txt", "r") as f:
        lines = [line.strip() for line in f]

    wires = {}
    gates = []
    parsing_wires = True

    for line in lines:
        if not line:
            parsing_wires = False
            continue
        if parsing_wires:
            match = re.match(r"(\w+):\s*([01])", line)
            if match:
                wires[match.group(1)] = int(match.group(2))
        else:
            match = re.match(r"(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)", line)
            if match:
                gates.append(match.groups())

    remaining_gates = gates
    while remaining_gates:
        progress = False
        new_remaining_gates = []
        for input1, op, input2, output in remaining_gates:
            if input1 in wires and input2 in wires:
                val1 = wires[input1]
                val2 = wires[input2]
                if op == "AND":
                    wires[output] = 1 if val1 == 1 and val2 == 1 else 0
                elif op == "OR":
                    wires[output] = 1 if val1 == 1 or val2 == 1 else 0
                elif op == "XOR":
                    wires[output] = 1 if val1 != val2 else 0
                progress = True
            else:
                new_remaining_gates.append((input1, op, input2, output))
        if not progress:
            return "Cannot evaluate remaining gates due to missing inputs or cyclic dependencies."
        remaining_gates = new_remaining_gates

    z_wires = {int(re.match(r"z(\d+)", wire).group(1)): val for wire, val in wires.items() if re.match(r"z\d+", wire)}
    if not z_wires:
        return "No wires starting with 'z' found."

    binary_string = "".join(str(z_wires[i]) for i in sorted(z_wires, reverse=True))
    return int(binary_string, 2)

print(solve())
