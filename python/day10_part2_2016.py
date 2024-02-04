
import re

class Bot:
    def __init__(self):
        self.lowTo = None
        self.highTo = None
        self.chips = []

bots = {}
outputs = {}
valueRegex = re.compile(r'value (\d+) goes to (bot \d+)')
givesRegex = re.compile(r'(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)')

with open('input.txt', 'r') as file:
    for line in file:
        if valueRegex.match(line):
            value, botID = map(str, valueRegex.findall(line)[0])
            value = int(value)
            if botID not in bots:
                bots[botID] = Bot()
            bots[botID].chips.append(value)
        elif givesRegex.match(line):
            botID, lowTo, highTo = map(str, givesRegex.findall(line)[0])
            if botID not in bots:
                bots[botID] = Bot()
            bots[botID].lowTo = lowTo
            bots[botID].highTo = highTo

def give_chip(target, value):
    if target.startswith('bot'):
        if target not in bots:
            bots[target] = Bot()
        bots[target].chips.append(value)
    elif target.startswith('output'):
        outputs[target] = value

def min_max(a, b):
    return (a, b) if a < b else (b, a)

while True:
    action = False
    for b in bots.values():
        if len(b.chips) == 2:
            action = True
            low, high = min_max(b.chips[0], b.chips[1])
            b.chips = []
            give_chip(b.lowTo, low)
            give_chip(b.highTo, high)
    if not action:
        break

result = outputs['output 0'] * outputs['output 1'] * outputs['output 2']
print(result)
