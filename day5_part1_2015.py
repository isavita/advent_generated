with open("input.txt", "r") as file:
    strings = file.read().splitlines()

nice_count = 0

for s in strings:
    vowels = sum([1 for c in s if c in "aeiou"])
    double_letter = any([s[i] == s[i+1] for i in range(len(s)-1)])
    forbidden_strings = ["ab", "cd", "pq", "xy"]
    forbidden = any([fs in s for fs in forbidden_strings])
    
    if vowels >= 3 and double_letter and not forbidden:
        nice_count += 1

print(nice_count)