
sum = 0

with open("input.txt", "r") as file:
    for line in file:
        if line.strip() == "":
            continue
        first_digit, last_digit = -1, -1
        for char in line:
            if char.isdigit():
                if first_digit == -1:
                    first_digit = int(char)
                last_digit = int(char)
        if first_digit != -1 and last_digit != -1:
            value = int(str(first_digit) + str(last_digit))
            sum += value

print(sum)
