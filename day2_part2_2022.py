
total_score = 0

with open("input.txt", "r") as file:
    for line in file:
        opponent, round_end = line[0], line[2]

        your_move = ' '
        if round_end == 'X':
            if opponent == 'A':
                your_move = 'Z'
            elif opponent == 'B':
                your_move = 'X'
            else:
                your_move = 'Y'
        elif round_end == 'Y':
            if opponent == 'A':
                your_move = 'X'
            elif opponent == 'B':
                your_move = 'Y'
            else:
                your_move = 'Z'
        else:
            if opponent == 'A':
                your_move = 'Y'
            elif opponent == 'B':
                your_move = 'Z'
            else:
                your_move = 'X'

        score = 0
        if your_move == 'X':
            score = 1
        elif your_move == 'Y':
            score = 2
        elif your_move == 'Z':
            score = 3

        if (opponent == 'A' and your_move == 'Y') or (opponent == 'B' and your_move == 'Z') or (opponent == 'C' and your_move == 'X'):
            score += 6
        elif opponent == 'A' and your_move == 'X' or opponent == 'B' and your_move == 'Y' or opponent == 'C' and your_move == 'Z':
            score += 3

        total_score += score

print(total_score)
