
from collections import Counter

with open("input.txt", "r") as file:
    lines = file.read().strip().split('\n')

message_length = len(lines[0])
message = ""

for i in range(message_length):
    column = [line[i] for line in lines]
    most_common = Counter(column).most_common(1)[0][0]
    message += most_common

print(message)
