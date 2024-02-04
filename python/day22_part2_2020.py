
class Deck:
    def __init__(self, cards):
        self.cards = cards

    def copy(self, n):
        return Deck(self.cards[:n])

    def score(self):
        return sum(card * (len(self.cards) - i) for i, card in enumerate(self.cards))

def play_recursive_combat(player1, player2):
    previous_rounds = set()
    while player1.cards and player2.cards:
        round_key = f"{player1.cards}|{player2.cards}"
        if round_key in previous_rounds:
            return player1, Deck([])
        previous_rounds.add(round_key)

        card1, card2 = player1.cards.pop(0), player2.cards.pop(0)

        if len(player1.cards) >= card1 and len(player2.cards) >= card2:
            sub_player1, _ = play_recursive_combat(player1.copy(card1), player2.copy(card2))
            if sub_player1.cards:
                player1.cards.extend([card1, card2])
            else:
                player2.cards.extend([card2, card1])
        else:
            if card1 > card2:
                player1.cards.extend([card1, card2])
            else:
                player2.cards.extend([card2, card1])

    return player1, player2

with open("input.txt", "r") as file:
    lines = file.readlines()
    player1_deck = Deck([])
    player2_deck = Deck([])
    current_deck = player1_deck

    for line in lines:
        line = line.strip()
        if not line:
            current_deck = player2_deck
            continue
        if "Player" in line:
            continue
        card = int(line)
        current_deck.cards.append(card)

player1_deck, player2_deck = play_recursive_combat(player1_deck, player2_deck)

winning_deck = player1_deck if player1_deck.cards else player2_deck

print(winning_deck.score())
