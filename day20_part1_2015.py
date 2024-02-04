with open("input.txt", "r") as file:
    puzzle_input = int(file.read().strip())

def factors_sum(n):
    total = 0
    for i in range(1, int(n**0.5) + 1):
        if n % i == 0:
            if n / i == i:
                total += i
            else:
                total += i + n/i
    return total

house_num = 1
while factors_sum(house_num) * 10 < puzzle_input:
    house_num += 1

print(house_num)