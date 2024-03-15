from typing import Tuple

def parse_input(input_str: str) -> Tuple[int, int]:
    player1_pos, player2_pos = 0, 0
    for line in input_str.splitlines():
        if line.startswith("Player 1"):
            player1_pos = int(line.split(": ")[1])
        elif line.startswith("Player 2"):
            player2_pos = int(line.split(": ")[1])
    return player1_pos, player2_pos

def play(positions: Tuple[int, int], scores: Tuple[int, int], rolls_left: int, is_player1_turn: bool, memo: dict) -> Tuple[int, int]:
    key = str((positions, scores, rolls_left, is_player1_turn))
    if key in memo:
        return memo[key]

    player_index = 0 if is_player1_turn else 1
    scores_copy = list(scores)

    if rolls_left == 0:
        scores_copy[player_index] += positions[player_index]
        if scores_copy[player_index] >= 21:
            return (1, 0) if player_index == 0 else (0, 1)
        is_player1_turn = not is_player1_turn
        rolls_left = 3
        player_index = (player_index + 1) % 2

    wins1, wins2 = 0, 0
    for roll in range(1, 4):
        positions_copy = list(positions)
        positions_copy[player_index] = (positions_copy[player_index] + roll - 1) % 10 + 1
        r1, r2 = play(tuple(positions_copy), tuple(scores_copy), rolls_left - 1, is_player1_turn, memo)
        wins1 += r1
        wins2 += r2

    memo[key] = (wins1, wins2)
    return wins1, wins2

def solve(input_str: str) -> int:
    player1_pos, player2_pos = parse_input(input_str)
    wins1, wins2 = play((player1_pos, player2_pos), (0, 0), 3, True, {})
    return max(wins1, wins2)

with open("input.txt", "r") as f:
    input_str = f.read().strip()
print(solve(input_str))