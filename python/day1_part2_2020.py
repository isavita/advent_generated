
from itertools import combinations

expenses = []
with open('input.txt', 'r') as file:
    for line in file:
        expenses.append(int(line.strip()))

# Part One
for expense1, expense2 in combinations(expenses, 2):
    if expense1 + expense2 == 2020:
        print(expense1 * expense2)

# Part Two
for expense1, expense2, expense3 in combinations(expenses, 3):
    if expense1 + expense2 + expense3 == 2020:
        print(expense1 * expense2 * expense3)
