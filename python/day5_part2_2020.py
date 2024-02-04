
with open("input.txt") as f:
    passes = f.read().strip().split('\n')

def get_seat_id(boarding_pass):
    row = int(boarding_pass[:7].replace('F', '0').replace('B', '1'), 2)
    col = int(boarding_pass[7:].replace('L', '0').replace('R', '1'), 2)
    return row * 8 + col

seat_ids = [get_seat_id(pass_) for pass_ in passes]

print(max(seat_ids))

possible_seats = set(range(min(seat_ids), max(seat_ids)+1))
missing_seat = possible_seats - set(seat_ids)

print(missing_seat.pop())
