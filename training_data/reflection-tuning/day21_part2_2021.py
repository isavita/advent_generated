from functools import lru_cache

def read_input(filename):
    with open(filename, 'r') as file:
        p1_start = int(file.readline().split()[-1])
        p2_start = int(file.readline().split()[-1])
    return p1_start, p2_start

@lru_cache(maxsize=None)
def play_dirac(p1_pos, p2_pos, p1_score, p2_score, p1_turn):
    if p1_score >= 21:
        return 1, 0
    if p2_score >= 21:
        return 0, 1

    p1_wins, p2_wins = 0, 0

    for d1 in range(1, 4):
        for d2 in range(1, 4):
            for d3 in range(1, 4):
                if p1_turn:
                    new_p1_pos = (p1_pos + d1 + d2 + d3 - 1) % 10 + 1
                    new_p1_score = p1_score + new_p1_pos
                    w1, w2 = play_dirac(new_p1_pos, p2_pos, new_p1_score, p2_score, False)
                else:
                    new_p2_pos = (p2_pos + d1 + d2 + d3 - 1) % 10 + 1
                    new_p2_score = p2_score + new_p2_pos
                    w1, w2 = play_dirac(p1_pos, new_p2_pos, p1_score, new_p2_score, True)
                
                p1_wins += w1
                p2_wins += w2

    return p1_wins, p2_wins

def main():
    p1_start, p2_start = read_input("input.txt")
    p1_wins, p2_wins = play_dirac(p1_start, p2_start, 0, 0, True)
    print(max(p1_wins, p2_wins))

if __name__ == "__main__":
    main()
