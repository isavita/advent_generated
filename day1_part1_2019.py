masses = [int(line.strip()) for line in open('input.txt')]
total_fuel = sum([(mass // 3 - 2) for mass in masses])
print(total_fuel)