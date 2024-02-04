import hashlib

with open('input.txt', 'r') as file:
    secret_key = file.read().strip()

number = 1
while True:
    input_string = secret_key + str(number)
    hash_result = hashlib.md5(input_string.encode()).hexdigest()
    if hash_result[:5] == '00000':
        print(number)
        break
    number += 1