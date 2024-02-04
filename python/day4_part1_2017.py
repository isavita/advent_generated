
with open('input.txt', 'r') as file:
    passphrases = file.readlines()

valid_count = 0

for passphrase in passphrases:
    words = passphrase.strip().split()
    if len(words) == len(set(words)):
        valid_count += 1

print(valid_count)
