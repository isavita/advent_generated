with open('input.txt') as f:
    modules = [int(line.strip()) for line in f]

def calculate_fuel(mass):
    fuel = mass // 3 - 2
    if fuel <= 0:
        return 0
    return fuel + calculate_fuel(fuel)

total_fuel = sum(calculate_fuel(module) for module in modules)
print(total_fuel)