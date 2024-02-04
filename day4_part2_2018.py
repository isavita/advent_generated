guards = {}
current_guard = None
falls_asleep = None

with open('input.txt') as f:
    lines = f.readlines()

lines.sort()

for line in lines:
    if '#' in line:
        current_guard = int(line.split()[3][1:])
        if current_guard not in guards:
            guards[current_guard] = [0] * 60
    elif 'falls asleep' in line:
        falls_asleep = int(line.split()[1][3:5])
    elif 'wakes up' in line:
        wakes_up = int(line.split()[1][3:5])
        for i in range(falls_asleep, wakes_up):
            guards[current_guard][i] += 1

max_sleep = max(sum(guards[guard]) for guard in guards)
sleepiest_guard = [guard for guard in guards if sum(guards[guard]) == max_sleep][0]
most_common_minute = guards[sleepiest_guard].index(max(guards[sleepiest_guard]))

result_part1 = sleepiest_guard * most_common_minute
print(result_part1)

most_frequent_minute = max((max(guards[guard]), guard, guards[guard].index(max(guards[guard]))) for guard in guards)
result_part2 = most_frequent_minute[1] * most_frequent_minute[2]
print(result_part2)