
with open("input.txt", "r") as file:
    data = file.read().splitlines()

def seat_id(boarding_pass):
    row = int(boarding_pass[:7].replace("F", "0").replace("B", "1"), 2)
    col = int(boarding_pass[7:].replace("L", "0").replace("R", "1"), 2)
    return row * 8 + col

seat_ids = [seat_id(boarding_pass) for boarding_pass in data]
print(max(seat_ids))
