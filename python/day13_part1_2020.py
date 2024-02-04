
with open("input.txt") as file:
    timestamp = int(file.readline())
    buses = [int(bus) for bus in file.readline().split(",") if bus != "x"]

min_wait = float('inf')
selected_bus = None

for bus in buses:
    wait_time = bus - (timestamp % bus)
    if wait_time < min_wait:
        min_wait = wait_time
        selected_bus = bus

print(selected_bus * min_wait)
