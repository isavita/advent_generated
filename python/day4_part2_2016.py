
from collections import Counter

def is_real_room(name, checksum):
    name = name.replace('-', '')
    count = Counter(name)
    most_common = sorted(count, key=lambda x: (-count[x], x))[:5]
    return ''.join(most_common) == checksum

def decrypt_name(name, sector_id):
    decrypted = ''
    for char in name:
        if char == '-':
            decrypted += ' '
        else:
            shift = sector_id % 26
            decrypted += chr(((ord(char) - ord('a') + shift) % 26) + ord('a'))
    return decrypted

total_sector_ids = 0

with open('input.txt', 'r') as file:
    for line in file:
        parts = line.strip().split('-')
        name = '-'.join(parts[:-1])
        sector_id, checksum = parts[-1].split('[')
        sector_id = int(sector_id)
        checksum = checksum[:-1]
        
        if is_real_room(name, checksum):
            total_sector_ids += sector_id
            
            if decrypt_name(name, sector_id) == 'northpole object storage':
                print(sector_id)

print(total_sector_ids)
