with open('input.txt') as f:
    data = f.readlines()

valid_count = 0
for line in data:
    policy, password = line.split(': ')
    positions, letter = policy.split(' ')
    pos1, pos2 = map(int, positions.split('-'))
    
    if (password[pos1 - 1] == letter) ^ (password[pos2 - 1] == letter):
        valid_count += 1

print(valid_count)