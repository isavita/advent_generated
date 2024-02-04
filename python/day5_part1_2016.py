import hashlib

with open("input.txt", "r") as file:
    door_id = file.read().strip()

password = ""
index = 0

while len(password) < 8:
    hash_input = door_id + str(index)
    result = hashlib.md5(hash_input.encode()).hexdigest()
    
    if result[:5] == "00000":
        password += result[5]
    
    index += 1

print(password)