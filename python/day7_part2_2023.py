
import re
import os
import sys

valueDict = {'J': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9, 'T': 10, 'Q': 11, 'K': 12, 'A': 13}

class Hand:
    def __init__(self, cards, bid):
        self.cards = cards
        self.bid = bid

with open("input.txt", "r") as file:
    input_data = file.read()

lines = input_data.split("\n")
hands = []

re_pattern = r'[\dAKQJT]+'
bid_re_pattern = r' [\d]+'

for line in lines:
    if len(line) == 0:
        continue

    cards = re.search(re_pattern, line).group()
    bid = int(re.search(bid_re_pattern, line).group()[1:])

    hands.append(Hand(cards, bid))

matches = [[], [], [], [], [], [], []]

for hand in hands:
    count = {}

    for i in hand.cards:
        if i in count:
            count[i] += 1
        else:
            count[i] = 1

    if 'J' in count and count['J'] > 0:
        high_v = 0
        high_key = 'J'
        for y in count:
            if y != 'J':
                if count[y] > high_v:
                    high_key = y
                    high_v = count[y]
                elif count[y] == high_v and valueDict[y] > valueDict[high_key]:
                    high_key = y
        if high_key != 'J':
            count[high_key] += count['J']
            del count['J']

    value = 1
    for i in count.values():
        value *= i

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

converted_matches = []

for x in matches:
    temp = []
    for i in x:
        y = i.cards.replace("A", "E").replace("T", "A").replace("J", "1").replace("Q", "C").replace("K", "D")
        val = int(y, 16)
        temp.append([val, i.bid])
    temp.sort(key=lambda x: x[0], reverse=True)
    converted_matches.extend(temp)

total = 0
for x in range(len(converted_matches)):
    total += converted_matches[x][1] * (len(converted_matches) - x)

print(total)
