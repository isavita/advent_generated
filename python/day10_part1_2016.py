
import re

def min_max(a, b):
    return (a, b) if a < b else (b, a)

bots = {}
value_regex = re.compile(r'value (\d+) goes to (bot \d+)')
gives_regex = re.compile(r'(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)')

with open("input.txt") as file:
    for line in file:
        if value_regex.match(line):
            value, bot_id = map(str, value_regex.findall(line)[0])
            if bot_id not in bots:
                bots[bot_id] = {"lowTo": "", "highTo": "", "chips": []}
            bots[bot_id]["chips"].append(int(value))
        elif gives_regex.match(line):
            bot_id, low_to, high_to = map(str, gives_regex.findall(line)[0])
            if bot_id not in bots:
                bots[bot_id] = {"lowTo": "", "highTo": "", "chips": []}
            bots[bot_id]["lowTo"] = low_to
            bots[bot_id]["highTo"] = high_to

while True:
    action = False
    for bot_id, b in bots.items():
        if len(b["chips"]) == 2:
            action = True
            low, high = min_max(b["chips"][0], b["chips"][1])
            if low == 17 and high == 61:
                print(bot_id)
                exit()
            b["chips"] = []

            if b["lowTo"]:
                if b["lowTo"] not in bots:
                    bots[b["lowTo"]] = {"lowTo": "", "highTo": "", "chips": []}
                bots[b["lowTo"]]["chips"].append(low)

            if b["highTo"]:
                if b["highTo"] not in bots:
                    bots[b["highTo"]] = {"lowTo": "", "highTo": "", "chips": []}
                bots[b["highTo"]]["chips"].append(high)

    if not action:
        break
