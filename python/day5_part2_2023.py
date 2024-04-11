def reverse_convert_number(number, ranges):
    for r in reversed(ranges):
        if r['destStart'] <= number < r['destStart'] + r['length']:
            return r['srcStart'] + (number - r['destStart'])
    return number

def is_in_seed_ranges(number, seed_ranges):
    for r in seed_ranges:
        if r[0] <= number < r[0] + r[1]:
            return True
    return False

def main():
    with open("input.txt", "r") as file:
        lines = file.readlines()

    seed_ranges = []
    current_ranges = []
    maps = []

    for line in lines:
        line = line.strip()
        if "map:" in line:
            if current_ranges:
                maps.append(current_ranges)
                current_ranges = []
        elif line.startswith("seeds:"):
            seed_info = line[7:].split()
            for i in range(0, len(seed_info), 2):
                start = int(seed_info[i])
                length = int(seed_info[i+1])
                seed_ranges.append((start, length))
        else:
            numbers = line.split()
            if len(numbers) == 3:
                destStart = int(numbers[0])
                srcStart = int(numbers[1])
                length = int(numbers[2])
                current_ranges.append({'srcStart': srcStart, 'destStart': destStart, 'length': length})

    if current_ranges:
        maps.append(current_ranges)

    location = 0
    while True:
        seed = location
        for m in reversed(maps):
            seed = reverse_convert_number(seed, m)

        if is_in_seed_ranges(seed, seed_ranges):
            print(location)
            break

        location += 1

if __name__ == "__main__":
    main()