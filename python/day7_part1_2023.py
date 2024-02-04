
import re
import sys

HighCard = 1
OnePair = 2
TwoPair = 3
ThreeKind = 4
FullHouse = 5
FourKind = 6
FiveKind = 7

matches = [[], [], [], [], [], [], []]

class Hand:
    def __init__(self, cards, bid):
        self.cards = cards
        self.bid = bid

class RankedHand:
    def __init__(self, hand, rank):
        self.hand = hand
        self.rank = rank

def findMatches(hands):
    for hand in hands:
        count = {}
        for card in hand.cards:
            count[card] = count.get(card, 0) + 1
        value = 1
        for c in count.values():
            value *= c
        if value == 1:
            matches[6].append(hand)
        elif value == 2:
            matches[5].append(hand)
        elif value == 3:
            matches[3].append(hand)
        elif value == 4:
            if len(count) == 2:
                matches[1].append(hand)
            else:
                matches[4].append(hand)
        elif value == 5:
            matches[0].append(hand)
        elif value == 6:
            matches[2].append(hand)
        else:
            print("oh no")

def convertAndOrderMatches():
    convertedMatches = []
    for category in matches:
        temp = []
        for hand in category:
            cards = re.sub('A', 'E', hand.cards)
            cards = re.sub('T', 'A', cards)
            cards = re.sub('J', 'B', cards)
            cards = re.sub('Q', 'C', cards)
            cards = re.sub('K', 'D', cards)
            num = int(cards, 16)
            temp.append(RankedHand(hand, num))
        temp.sort(key=lambda x: x.rank, reverse=True)
        convertedMatches.extend(temp)
    return convertedMatches

file = open("input.txt", "r")
lines = file.read()
file.close()

hands = []
re_cards = re.compile(r'[\dAKQJT]+')
re_bid = re.compile(r' [\d]+')

for line in re.split('\n', lines):
    if len(line) == 0:
        continue
    cards = re_cards.search(line).group(0)
    bid = int(re_bid.search(line).group(0)[1:])
    hands.append(Hand(cards, bid))

findMatches(hands)
convertedMatches = convertAndOrderMatches()

total = 0
for i, ranked_hand in enumerate(convertedMatches):
    total += ranked_hand.hand.bid * (len(convertedMatches) - i)

print(total)
