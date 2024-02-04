
import hashlib

with open("input.txt", "r") as file:
    salt = file.read().strip()

keys = 0
index = 0

while keys < 64:
    hash_str = hashlib.md5((salt + str(index)).encode()).hexdigest()
    triplet = next((hash_str[i] for i in range(len(hash_str)-2) if hash_str[i] == hash_str[i+1] == hash_str[i+2]), None)
    
    if triplet:
        for i in range(1, 1001):
            next_hash_str = hashlib.md5((salt + str(index+i)).encode()).hexdigest()
            if triplet * 5 in next_hash_str:
                keys += 1
                break
    
    index += 1

print(index - 1)
