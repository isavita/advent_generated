from collections import deque, defaultdict

def parse_input(input_str):
    modules = {}
    for line in input_str.strip().split('\n'):
        name, destinations = line.split(' -> ')
        destinations = destinations.split(', ')
        if name == 'broadcaster':
            modules[name] = ('broadcaster', destinations)
        elif name.startswith('%'):
            modules[name[1:]] = ('flip-flop', destinations, False)  # False for off
        elif name.startswith('&'):
            modules[name[1:]] = ('conjunction', destinations, {})
    
    # Initialize conjunction inputs
    for name, (type, destinations, _) in modules.items():
        for dest in destinations:
            if dest in modules and modules[dest][0] == 'conjunction':
                modules[dest][2][name] = False  # Initialize with low pulse
    
    return modules

def simulate_button_press(modules):
    low_pulses, high_pulses = 1, 0  # Start with one low pulse from button
    queue = deque([('broadcaster', False, 'button')])
    
    while queue:
        module, pulse, sender = queue.popleft()
        
        if module not in modules:
            continue
        
        type, destinations, state = modules[module]
        
        if type == 'broadcaster':
            for dest in destinations:
                queue.append((dest, pulse, module))
                if pulse:
                    high_pulses += 1
                else:
                    low_pulses += 1
        
        elif type == 'flip-flop':
            if not pulse:  # Only react to low pulses
                new_pulse = not state
                modules[module] = (type, destinations, new_pulse)
                for dest in destinations:
                    queue.append((dest, new_pulse, module))
                    if new_pulse:
                        high_pulses += 1
                    else:
                        low_pulses += 1
        
        elif type == 'conjunction':
            state[sender] = pulse
            new_pulse = not all(state.values())
            for dest in destinations:
                queue.append((dest, new_pulse, module))
                if new_pulse:
                    high_pulses += 1
                else:
                    low_pulses += 1
    
    return low_pulses, high_pulses

def solve(input_str):
    modules = parse_input(input_str)
    total_low, total_high = 0, 0
    
    for _ in range(1000):
        low, high = simulate_button_press(modules)
        total_low += low
        total_high += high
    
    return total_low * total_high

# Example usage:
input_str = """
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"""
result = solve(input_str)
print(result)
