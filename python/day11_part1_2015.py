
import re

def has_straight(s):
    for i in range(len(s) - 2):
        if ord(s[i]) == ord(s[i+1]) - 1 and ord(s[i+1]) == ord(s[i+2]) - 1:
            return True
    return False

def has_no_forbidden_letters(s):
    return 'i' not in s and 'o' not in s and 'l' not in s

def has_two_non_overlapping_pairs(s):
    return len(re.findall(r'(.)\1', s)) >= 2

def increment_password(s):
    s = list(s)
    i = len(s) - 1
    while i >= 0:
        if s[i] == 'z':
            s[i] = 'a'
            i -= 1
        else:
            s[i] = chr(ord(s[i]) + 1)
            break
    return ''.join(s)

def find_next_password(s):
    while True:
        s = increment_password(s)
        if has_straight(s) and has_no_forbidden_letters(s) and has_two_non_overlapping_pairs(s):
            return s

with open('input.txt', 'r') as file:
    current_password = file.read().strip()

next_password = find_next_password(current_password)
print(next_password)
