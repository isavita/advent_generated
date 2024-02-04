
class RangeMap:
    def __init__(self, src_start, dest_start, length):
        self.src_start = src_start
        self.dest_start = dest_start
        self.length = length

def convert_number(number, ranges):
    for r in ranges:
        if number >= r.src_start and number < r.src_start + r.length:
            return r.dest_start + (number - r.src_start)
    return number

seeds = []
current_ranges = []
maps = []

with open("input.txt", "r") as file:
    for line in file:
        if "map:" in line:
            if current_ranges:
                maps.append(current_ranges)
                current_ranges = []
        elif line.startswith("seeds:"):
            seed_strs = line[7:].split()
            for s in seed_strs:
                seeds.append(int(s))
        else:
            numbers = line.split()
            if len(numbers) == 3:
                src_start = int(numbers[1])
                dest_start = int(numbers[0])
                length = int(numbers[2])
                current_ranges.append(RangeMap(src_start, dest_start, length))
    maps.append(current_ranges)

min_location = -1
for seed in seeds:
    location = seed
    for m in maps:
        location = convert_number(location, m)
    if min_location == -1 or location < min_location:
        min_location = location

print(min_location)
