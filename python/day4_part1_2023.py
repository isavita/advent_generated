def convert_to_int_list(string):
    return [int(num) for num in string.split() if num.isdigit()]

def calculate_points(winning_numbers, your_numbers):
    points = 0
    for num in your_numbers:
        if num in winning_numbers:
            if points == 0:
                points = 1
            else:
                points *= 2
    return points

with open("input.txt", "r") as file:
    total_points = 0
    for line in file:
        parts = line.strip().split(" | ")
        if len(parts) == 2:
            winning_numbers = convert_to_int_list(parts[0])
            your_numbers = convert_to_int_list(parts[1])
            total_points += calculate_points(winning_numbers, your_numbers)

print(total_points)