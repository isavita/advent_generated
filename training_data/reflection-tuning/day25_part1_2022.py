def snafu_to_decimal(snafu):
    snafu_map = {'2': 2, '1': 1, '0': 0, '-': -1, '=': -2}
    decimal = 0
    for i, digit in enumerate(reversed(snafu)):
        decimal += snafu_map[digit] * (5 ** i)
    return decimal

def decimal_to_snafu(decimal):
    if decimal == 0:
        return '0'
    snafu = []
    while decimal:
        remainder = decimal % 5
        decimal //= 5
        if remainder <= 2:
            snafu.append(str(remainder))
        else:
            snafu.append('=' if remainder == 3 else '-')
            decimal += 1
    return ''.join(reversed(snafu))

# Read input and calculate sum
with open('input.txt', 'r') as file:
    total = sum(snafu_to_decimal(line.strip()) for line in file)

# Convert sum to SNAFU and print result
result = decimal_to_snafu(total)
print(result)
