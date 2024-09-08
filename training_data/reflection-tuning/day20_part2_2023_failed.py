from collections import deque, defaultdict

class Module:
    def __init__(self, name, type, destinations):
        self.name = name
        self.type = type
        self.destinations = destinations
        self.state = False
        self.inputs = {}

    def receive(self, pulse, source):
        if self.type == '%':
            if pulse == 'low':
                self.state = not self.state
                return [('high' if self.state else 'low', dest) for dest in self.destinations]
            return []
        elif self.type == '&':
            self.inputs[source] = pulse
            output = 'low' if all(p == 'high' for p in self.inputs.values()) else 'high'
            return [(output, dest) for dest in self.destinations]
        else:  # broadcaster
            return [(pulse, dest) for dest in self.destinations]

def parse_input(input_data):
    modules = {}
    for line in input_data.strip().split('\n'):
        name, destinations = line.split(' -> ')
        destinations = destinations.split(', ')
        if name == 'broadcaster':
            modules[name] = Module(name, 'broadcaster', destinations)
        else:
            type, name = name[0], name[1:]
            modules[name] = Module(name, type, destinations)
    
    for name, module in modules.items():
        for dest in module.destinations:
            if dest in modules and modules[dest].type == '&':
                modules[dest].inputs[name] = 'low'
    
    return modules

def simulate(modules, button_presses):
    low_count = high_count = 0
    for _ in range(button_presses):
        queue = deque([('low', 'broadcaster', 'button')])
        while queue:
            pulse, target, source = queue.popleft()
            if pulse == 'low':
                low_count += 1
            else:
                high_count += 1
            
            if target not in modules:
                continue
            
            new_pulses = modules[target].receive(pulse, source)
            queue.extend((p, d, target) for p, d in new_pulses)
    
    return low_count, high_count

def find_rx_cycle(modules):
    cycle = 0
    rx_inputs = [name for name, module in modules.items() if 'rx' in module.destinations]
    if not rx_inputs:
        return None
    rx_input = rx_inputs[0]
    required_highs = set(modules[rx_input].inputs.keys())
    seen_highs = defaultdict(int)

    while True:
        cycle += 1
        queue = deque([('low', 'broadcaster', 'button')])
        while queue:
            pulse, target, source = queue.popleft()
            if target not in modules:
                continue
            
            if target == rx_input and pulse == 'high':
                seen_highs[source] = cycle
                if seen_highs.keys() == required_highs:
                    return lcm(*seen_highs.values())
            
            new_pulses = modules[target].receive(pulse, source)
            queue.extend((p, d, target) for p, d in new_pulses)

def lcm(a, b):
    return abs(a * b) // math.gcd(a, b)

def solve(input_data):
    modules = parse_input(input_data)
    
    # Part 1
    low, high = simulate(modules, 1000)
    part1 = low * high
    
    # Part 2
    part2 = find_rx_cycle(modules)
    
    return part1, part2

# Example usage
input_data = """
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"""
part1, part2 = solve(input_data)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
