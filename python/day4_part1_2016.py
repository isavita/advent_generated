
from collections import Counter

def is_real_room(name, checksum):
    name = name.replace('-', '')
    count = Counter(name)
    most_common = sorted(count, key=lambda x: (-count[x], x))[:5]
    return ''.join(most_common) == checksum

def decrypt_name(name, shift):
    decrypted = ''
    for char in name:
        if char == '-':
            decrypted += ' '
        else:
            decrypted += chr((ord(char) - ord('a') + shift) % 26 + ord('a'))
    return decrypted

total_sum = 0

with open('input.txt', 'r') as file:
    for line in file:
        parts = line.strip().split('-')
        name = '-'.join(parts[:-1])
        sector_id, checksum = parts[-1][:-1].split('[')
        
        if is_real_room(name, checksum):
            total_sum += int(sector_id)
            decrypted_name = decrypt_name(name, int(sector_id))

print(total_sum)
