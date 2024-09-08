def shuffle(deck_size, card_position, instructions):
    for instruction in instructions:
        if instruction.startswith("deal into new stack"):
            card_position = deck_size - 1 - card_position
        elif instruction.startswith("cut"):
            n = int(instruction.split()[-1])
            card_position = (card_position - n) % deck_size
        elif instruction.startswith("deal with increment"):
            n = int(instruction.split()[-1])
            card_position = (card_position * n) % deck_size
    return card_position

# Read instructions from file
with open("input.txt", "r") as f:
    instructions = f.read().strip().split("\n")

# Set up initial conditions
deck_size = 10007
card_position = 2019

# Perform the shuffle and get the final position
final_position = shuffle(deck_size, card_position, instructions)

print(final_position)
