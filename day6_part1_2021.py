with open("input.txt", "r") as file:
    fish_str = file.read().strip()

fishes = [0] * 9
fish_list = fish_str.split(",")
for fish_str in fish_list:
    fish = int(fish_str)
    fishes[fish] += 1

for day in range(1, 81):
    new_fish = fishes[0]
    for i in range(1, len(fishes)):
        fishes[i-1] = fishes[i]
    fishes[6] += new_fish
    fishes[8] = new_fish

total_fish = sum(fishes)
print(total_fish)