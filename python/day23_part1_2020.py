
def play_game(cups, moves):
    current = cups[0]
    for _ in range(moves):
        picked_up = [cups.pop((cups.index(current) + 1) % len(cups)), cups.pop((cups.index(current) + 1) % len(cups)), cups.pop((cups.index(current) + 1) % len(cups))]
        destination = current - 1 if current > 1 else max(cups)
        while destination in picked_up:
            destination = destination - 1 if destination > 1 else max(cups)
        destination_index = (cups.index(destination) + 1) % len(cups)
        cups = cups[:destination_index] + picked_up + cups[destination_index:]
        current = cups[(cups.index(current) + 1) % len(cups)]
    return cups

with open("input.txt", "r") as file:
    input_data = file.read().strip()

cups = [int(x) for x in input_data]
result = play_game(cups, 100)
print(''.join(str(x) for x in result[result.index(1)+1:] + result[:result.index(1)]))
