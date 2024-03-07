import hashlib

def md5_hash(input_str):
    """Return the MD5 hash of the input string."""
    return hashlib.md5(input_str.encode()).hexdigest()

def stretched_md5_hash(input_str):
    """Return the MD5 hash of the input string, stretched over 2016 additional hashings."""
    hash_result = md5_hash(input_str)
    for _ in range(2016):
        hash_result = md5_hash(hash_result)
    return hash_result

def find_keys(salt, key_index=64):
    """Find the index that produces the specified key using the given salt."""
    keys_found = 0
    i = 0
    potential_keys = {}
    confirmed_keys = []

    while keys_found < key_index:
        hash_result = stretched_md5_hash(f"{salt}{i}")
        # Check if this hash confirms any potential keys.
        for char in set(hash_result):
            if char * 5 in hash_result:
                for potential_index in list(potential_keys.get(char, [])):
                    if i - potential_index <= 1000:
                        confirmed_keys.append(potential_index)
                        keys_found += 1
                        if keys_found == key_index:
                            return sorted(confirmed_keys)[key_index-1]
                potential_keys[char] = [index for index in potential_keys.get(char, []) if i - index < 1000]

        # Check for three consecutive characters.
        for j in range(len(hash_result) - 2):
            if hash_result[j] == hash_result[j+1] == hash_result[j+2]:
                potential_keys.setdefault(hash_result[j], []).append(i)
                break

        i += 1

# Read the salt from input.txt and find the index of the 64th key.
with open('input.txt', 'r') as file:
    salt = file.read().strip()

index_of_64th_key = find_keys(salt)
print(index_of_64th_key)
