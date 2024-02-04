
from collections import Counter

def calculate_rate(numbers, common=True):
    rate = ''
    for i in range(len(numbers[0])):
        bits = [num[i] for num in numbers]
        if common:
            rate += '1' if Counter(bits)['1'] > Counter(bits)['0'] else '0'
        else:
            rate += '1' if Counter(bits)['1'] < Counter(bits)['0'] else '0'
    return int(rate, 2)

with open('input.txt', 'r') as file:
    data = file.read().splitlines()

gamma_rate = calculate_rate(data, common=True)
epsilon_rate = calculate_rate(data, common=False)

power_consumption = gamma_rate * epsilon_rate
print(power_consumption)
