
from collections import defaultdict
import re

def parse_input(input_file):
    with open(input_file, 'r') as file:
        lines = file.readlines()
    
    records = []
    for line in lines:
        timestamp, action = re.match(r'\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (.+)', line).groups()
        records.append((timestamp, action))
    
    records.sort()
    
    guard_id = None
    asleep_minute = None
    guards_sleep = defaultdict(int)
    guards_minutes = defaultdict(lambda: defaultdict(int))
    
    for timestamp, action in records:
        if 'Guard' in action:
            guard_id = int(re.search(r'\d+', action).group())
        elif 'falls asleep' in action:
            asleep_minute = int(timestamp[-2:])
        elif 'wakes up' in action:
            for minute in range(asleep_minute, int(timestamp[-2:])):
                guards_sleep[guard_id] += 1
                guards_minutes[guard_id][minute] += 1
    
    most_asleep_guard = max(guards_sleep, key=guards_sleep.get)
    most_asleep_minute = max(guards_minutes[most_asleep_guard], key=guards_minutes[most_asleep_guard].get)
    
    return most_asleep_guard * most_asleep_minute

print(parse_input('input.txt'))
