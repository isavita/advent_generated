
def find_first_and_last_digit(line):
    digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

    first_digit, last_digit = 0, 0
    for i, char in enumerate(line):
        digit_str = char
        if digit_str.isdigit():
            if first_digit == 0:
                first_digit = int(digit_str)
            last_digit = int(digit_str)
        else:
            for j, digit in enumerate(digits):
                if line[i:].startswith(digit):
                    if first_digit == 0:
                        first_digit = j
                    last_digit = j
                    break

    return first_digit, last_digit

sum = 0
with open("input.txt", "r") as file:
    for line in file:
        first_digit, last_digit = find_first_and_last_digit(line)
        sum += 10 * first_digit + last_digit

print(sum)
