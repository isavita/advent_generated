
with open("input.txt", "r") as file:
    data = file.read().strip().split("\n\n")

calories = [sum(map(int, group.split())) for group in data]
result = max(calories)

print(result)
