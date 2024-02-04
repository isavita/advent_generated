
def transform_subject_number(subject_number, loop_size):
    value = 1
    for _ in range(loop_size):
        value *= subject_number
        value %= 20201227
    return value

def find_loop_size(public_key):
    value = 1
    subject_number = 7
    loop_size = 0
    while value != public_key:
        loop_size += 1
        value *= subject_number
        value %= 20201227
    return loop_size

with open('input.txt') as f:
    card_public_key, door_public_key = [int(x) for x in f.read().strip().split('\n')]

card_loop_size = find_loop_size(card_public_key)
door_loop_size = find_loop_size(door_public_key)

encryption_key1 = transform_subject_number(door_public_key, card_loop_size)
encryption_key2 = transform_subject_number(card_public_key, door_loop_size)

print(encryption_key1)
