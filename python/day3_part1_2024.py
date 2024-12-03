import re

with open('input.txt', 'r') as file:
    total_sum = 0
    for line in file:
        # Updated regex pattern to be more flexible with whitespace and handle potential non-digit characters
        matches = re.findall(r'mul\s*\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)', line)
        
        # Calculate sum of multiplications
        for num1, num2 in matches:
            total_sum += int(num1) * int(num2)
    
    print(f"Total sum: {total_sum}")
