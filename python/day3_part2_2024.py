import re

with open('input.txt', 'r') as file:
    content = file.read()
    
    pattern = r'(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don\'t\(\))'
    
    enabled = True
    total_sum = 0
    
    for match in re.finditer(pattern, content):
        if match.group(1):  # mul operation
            if enabled:
                nums = match.group(1)[4:-1].split(',')
                result = int(nums[0]) * int(nums[1])
                total_sum += result
                print(f"Adding {nums[0]} * {nums[1]} = {result}")
        elif match.group(2):  # do()
            enabled = True
            print("Multiplication enabled")
        elif match.group(3):  # don't()
            enabled = False
            print("Multiplication disabled")
    
    print(f"Total sum: {total_sum}")
