
def from_snafu(s):
    n = 0
    for char in s:
        n *= 5
        if char == '=':
            n -= 2
        elif char == '-':
            n -= 1
        else:
            n += ord(char) - ord('0')
    return n

def to_snafu(n):
    b = []
    while n > 0:
        remainder = n % 5
        if remainder == 3:
            n += 5
            b.append('=')
        elif remainder == 4:
            n += 5
            b.append('-')
        else:
            b.append(chr(ord('0') + remainder))
        n //= 5
    return ''.join(reversed(b))

def main():
    sum = 0
    with open("input.txt", "r") as file:
        for line in file:
            sum += from_snafu(line.strip())
    print(to_snafu(sum))

if __name__ == "__main__":
    main()
