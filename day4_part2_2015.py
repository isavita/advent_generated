import hashlib

def find_adventcoin(secret_key, num_zeroes):
    i = 1
    prefix = '0' * num_zeroes
    while True:
        key = secret_key + str(i)
        hash_key = hashlib.md5(key.encode()).hexdigest()
        if hash_key.startswith(prefix):
            return i
        i += 1

with open('input.txt', 'r') as file:
    secret_key = file.read().strip()

result_part1 = find_adventcoin(secret_key, 5)
result_part2 = find_adventcoin(secret_key, 6)

print(result_part1)
print(result_part2)