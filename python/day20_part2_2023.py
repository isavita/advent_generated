import os

BROADCASTER = 0
FLIP_FLOP = 1
CONJUNCTION = 2

class FlipFlop:
    def __init__(self, name, module_type, connects_to):
        self.name = name
        self.module_type = module_type
        self.connects_to = connects_to
        self.state = False

class Conjunction:
    def __init__(self, name, module_type, connects_to):
        self.name = name
        self.module_type = module_type
        self.connects_to = connects_to
        self.watches = {}

class Broadcaster:
    def __init__(self, name, module_type, connects_to):
        self.name = name
        self.module_type = module_type
        self.connects_to = connects_to

def handle_line(line, connections):
    if "broadcaster" in line:
        str_split = line.split(" -> ")
        module = Broadcaster(str_split[0], BROADCASTER, str_split[1].split(", "))
        connections[module.name] = module
    elif "%" in line:
        str_split = line.split(" -> ")
        module = FlipFlop(str_split[0][1:], FLIP_FLOP, str_split[1].split(", "))
        connections[module.name] = module
    else:
        str_split = line.split(" -> ")
        module = Conjunction(str_split[0][1:], CONJUNCTION, str_split[1].split(", "))
        connections[module.name] = module

def complete_watches(connections):
    for module in connections.values():
        if module.module_type == CONJUNCTION:
            for module2 in connections.values():
                if module2.module_type == FLIP_FLOP:
                    for name in module2.connects_to:
                        if name == module.name:
                            module.watches[module2.name] = False
                elif module2.module_type == CONJUNCTION:
                    for name in module2.connects_to:
                        if name == module.name:
                            module.watches[module2.name] = False

class State:
    def __init__(self, _from, name, pulse):
        self.from_ = _from
        self.name = name
        self.pulse = pulse

def simulate_press(connections, loops, press_number):
    queue = []
    original_connections = connections.copy()
    queue.append(State("button", "broadcaster", False))
    pulses = [1, 0]
    found = False

    while queue:
        curr_state = queue.pop(0)
        if curr_state.name not in connections:
            continue  # Skip if the module doesn't exist in the connections
        module = connections[curr_state.name]

        if curr_state.name == "out":
            continue

        if curr_state.name == "rx" and not curr_state.pulse:
            found = True

        pulse = curr_state.pulse

        if module.module_type == BROADCASTER:
            for name in module.connects_to:
                queue.append(State(module.name, name, pulse))
                if pulse:
                    pulses[1] += 1
                else:
                    pulses[0] += 1
            connections[curr_state.name] = module
        elif module.module_type == FLIP_FLOP:
            if not pulse:
                module.state = not module.state
                for name in module.connects_to:
                    connections[curr_state.name] = module
                    queue.append(State(module.name, name, module.state))
                    if module.state:
                        pulses[1] += 1
                    else:
                        pulses[0] += 1
            connections[curr_state.name] = module
        elif module.module_type == CONJUNCTION:
            module.watches[curr_state.from_] = pulse
            connections[curr_state.name] = module

            all_true = all(module.watches.values())
            for name in module.connects_to:
                queue.append(State(module.name, name, not all_true))
                if not all_true:
                    pulses[1] += 1
                else:
                    pulses[0] += 1
            connections[curr_state.name] = module

            if curr_state.name in loops and not all_true and loops[curr_state.name] == -1:
                loops[curr_state.name] = press_number

    return pulses, found

def sum_history(hist):
    total = [0, 0]
    for pulses in hist:
        total[0] += pulses[0]
        total[1] += pulses[1]
    return total[0] * total[1]

def connects_to(frm, to, connections):
    module = connections[frm]
    if module.module_type == BROADCASTER:
        return to in module.connects_to
    elif module.module_type == FLIP_FLOP:
        return to in module.connects_to
    elif module.module_type == CONJUNCTION:
        return to in module.connects_to
    return False

def copy_connections(connections):
    return connections.copy()

def included_in_history(hist, connections):
    for i, hist_connections in enumerate(hist):
        if len(hist_connections) != len(connections):
            continue
        if all(hist_connections[k].__dict__ == v.__dict__ for k, v in connections.items()):
            return i
    return -1

def main():
    with open("input.txt", "r") as file:
        lines = file.readlines()

    connections = {}
    for line in lines:
        handle_line(line.strip(), connections)

    complete_watches(connections)

    px_prev = [k for k, v in connections.items() if connects_to(k, "rx", connections)]
    if len(px_prev) != 1:
        raise ValueError("Error: more than one pxPrev")

    conj = connections[px_prev[0]]
    if conj.module_type != CONJUNCTION:
        raise ValueError("Error: pxPrev is not a conjunction")

    loop_lengths = {name: -1 for name in conj.watches}

    press_number = 0
    while True:
        press_number += 1
        pulses, found = simulate_press(connections, loop_lengths, press_number)
        if found:
            break
        if all(length != -1 for length in loop_lengths.values()):
            break

    result = 1
    for length in loop_lengths.values():
        result *= length

    print(result)

if __name__ == "__main__":
    main()