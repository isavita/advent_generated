with open("input.txt", "r") as file:
    passphrases = [line.strip().split() for line in file.readlines()]

# Part One
valid_passphrases = 0
for passphrase in passphrases:
    if len(passphrase) == len(set(passphrase)):
        valid_passphrases += 1

print(valid_passphrases)

# Part Two
def is_valid_passphrase(passphrase):
    sorted_words = [''.join(sorted(word)) for word in passphrase]
    return len(sorted_words) == len(set(sorted_words))

valid_passphrases = 0
for passphrase in passphrases:
    if is_valid_passphrase(passphrase):
        valid_passphrases += 1

print(valid_passphrases)