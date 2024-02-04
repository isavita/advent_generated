
import re

class Disc:
    def __init__(self, total_positions, start_position):
        self.total_positions = total_positions
        self.start_position = start_position

discs = []
with open("input.txt", "r") as file:
    disc_regex = re.compile(r"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).")
    for line in file:
        matches = disc_regex.match(line)
        total_positions = int(matches.group(2))
        start_position = int(matches.group(3))
        discs.append(Disc(total_positions, start_position))

time = 0
while True:
    if all((disc.start_position + time + i + 1) % disc.total_positions == 0 for i, disc in enumerate(discs)):
        print(time)
        break
    time += 1
