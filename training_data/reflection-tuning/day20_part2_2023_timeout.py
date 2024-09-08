from collections import deque
import math

class Module:
    def __init__(self, name, destinations):
        self.name = name
        self.destinations = destinations

    def receive(self, pulse, source):
        return []

class Broadcaster(Module):
    def receive(self, pulse, source):
        return [(dest, pulse, self.name) for dest in self.destinations]

class FlipFlop(Module):
    def __init__(self, name, destinations):
        super().__init__(name, destinations)
        self.state = False

    def receive(self, pulse, source):
        if pulse:
            return []
        self.state = not self.state
        return [(dest, self.state, self.name) for dest in self.destinations]

class Conjunction(Module):
    def __init__(self, name, destinations):
        super().__init__(name, destinations)
        self.memory = {}

    def receive(self, pulse, source):
        self.memory[source] = pulse
        output = not all(self.memory.values())
        return [(dest, output, self.name) for dest in self.destinations]

def parse_input(filename):
    modules = {}
    with open(filename, 'r') as file:
        for line in file:
            module, destinations = line.strip().split(' -> ')
            destinations = destinations.split(', ')
            if module == 'broadcaster':
                modules[module] = Broadcaster(module, destinations)
            elif module[0] == '%':
                modules[module[1:]] = FlipFlop(module[1:], destinations)
            elif module[0] == '&':
                modules[module[1:]] = Conjunction(module[1:], destinations)
    
    for module in modules.values():
        for dest in module.destinations:
            if dest in modules and isinstance(modules[dest], Conjunction):
                modules[dest].memory[module.name] = False
    
    return modules

def simulate(modules, button_presses, part2=False):
    low_pulses = high_pulses = 0
    rx_source = next(name for name, module in modules.items() if 'rx' in module.destinations)
    cycle_lengths = {input: 0 for input in modules[rx_source].memory}

    for press in range(1, button_presses + 1):
        queue = deque([('broadcaster', False, 'button')])
        while queue:
            target, pulse, source = queue.popleft()
            if pulse:
                high_pulses += 1
            else:
                low_pulses += 1

            if target not in modules:
                continue

            if part2 and target == rx_source and pulse:
                for input in modules[rx_source].memory:
                    if modules[rx_source].memory[input] and cycle_lengths[input] == 0:
                        cycle_lengths[input] = press

                if all(cycle_lengths.values()):
                    return math.lcm(*cycle_lengths.values())

            new_pulses = modules[target].receive(pulse, source)
            queue.extend(new_pulses)

    return low_pulses * high_pulses

modules = parse_input('input.txt')
print("Part 1:", simulate(modules, 1000))
print("Part 2:", simulate(modules, 100000000, True))
