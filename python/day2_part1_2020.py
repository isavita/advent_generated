
valid_passwords = 0

with open('input.txt', 'r') as file:
    for line in file:
        policy, password = line.strip().split(': ')
        limits, letter = policy.split(' ')
        min_count, max_count = map(int, limits.split('-'))
        
        count = password.count(letter)
        
        if min_count <= count <= max_count:
            valid_passwords += 1
            
print(valid_passwords)
