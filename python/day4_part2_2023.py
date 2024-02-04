
import re

class Card:
    def __init__(self, winnings, givens, totalCount):
        self.winnings = winnings
        self.givens = givens
        self.totalCount = totalCount

def getPointsForCard(card):
    points = 0
    for given, count in card.givens.items():
        if given in card.winnings:
            points += count * card.winnings[given]
    return points

def lexLineIntoCard(line):
    cardDataStr = line.split(": ")[1]
    cardData = cardDataStr.split(" | ")

    winnings = {}
    for point in re.findall(r"[0-9]{1,2}", cardData[0]):
        if point in winnings:
            winnings[point] += 1
        else:
            winnings[point] = 1

    givens = {}
    for point in re.findall(r"[0-9]{1,2}", cardData[1]):
        if point in givens:
            givens[point] += 1
        else:
            givens[point] = 1

    return Card(winnings, givens, 1)

with open("input.txt", "r") as file:
    input_data = file.read().strip()

cards = []
for line in input_data.split("\n"):
    if len(line) == 0:
        continue
    card = lexLineIntoCard(line)
    cards.append(card)

for i, card in enumerate(cards):
    points = getPointsForCard(card)

    for j in range(1, points+1):
        cards[i+j].totalCount += 1 * cards[i].totalCount

totalCards = sum(card.totalCount for card in cards)
print(totalCards)
