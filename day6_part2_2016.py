
from collections import Counter

with open('input.txt', 'r') as file:
    lines = file.read().splitlines()

message_len = len(lines[0])
message1 = ''
message2 = ''

for i in range(message_len):
    column = [line[i] for line in lines]
    most_common = Counter(column).most_common(1)[0][0]
    least_common = Counter(column).most_common()[-1][0]
    message1 += most_common
    message2 += least_common

print(message1)
print(message2)
