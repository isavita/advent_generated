with open('input.txt', 'r') as file:
    expenses = [int(line.strip()) for line in file]

for i in range(len(expenses)):
    for j in range(i+1, len(expenses)):
        if expenses[i] + expenses[j] == 2020:
            print(expenses[i] * expenses[j])
            break