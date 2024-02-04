
with open("input.txt", "r") as file:
    num_elves = int(file.read().strip())

def josephus(n):
    return int(bin(n)[3:] + '1', 2)

result = josephus(num_elves)
print(result)
