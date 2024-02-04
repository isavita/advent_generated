
import os
import sys
import string

def rtgHellDay(input_str, part):
    def newInitialState(input):
        s = {
            "floors": [[] for _ in range(4)],
            "elevatorLevel": 0,
            "steps": 0
        }

        for lineIndex, line in enumerate(input.split("\n")):
            parts = [v.strip(",.") for v in line.split(" ")]
            for i, word in enumerate(parts):
                if word == "generator":
                    material = parts[i-1]
                    s["floors"][lineIndex].append({"isChip": False, "material": material})
                elif word == "microchip":
                    material = parts[i-1][:parts[i-1].index("-comp")]
                    s["floors"][lineIndex].append({"isChip": True, "material": material})

        return s

    def hashKey(s):
        mapGenToIndex = {}
        mapChipToIndex = {}
        for flIndex, fl in enumerate(s["floors"]):
            for half in fl:
                if half["isChip"]:
                    mapChipToIndex[half["material"]] = flIndex
                else:
                    mapGenToIndex[half["material"]] = flIndex

        genChipPairs = []
        for material in mapGenToIndex.keys():
            genChipPairs.append([mapGenToIndex[material], mapChipToIndex[material]])

        genChipPairs.sort(key=lambda x: (x[0], x[1]))

        return str(s["elevatorLevel"]) + str(genChipPairs)

    def isValid(s):
        for i, fl in enumerate(s["floors"]):
            gensSeen = {}
            for half in fl:
                if not half["isChip"]:
                    gensSeen[half["material"]] = True

            if len(gensSeen) == 0:
                continue

            for half in fl:
                if half["isChip"] and not gensSeen.get(half["material"], False):
                    return False

        return True

    def isDone(s):
        lenSum = sum([len(fl) for fl in s["floors"][:3]])
        return lenSum == 0

    def getMovablePermIndices(s):
        permsToMove = []

        currentLevel = s["floors"][s["elevatorLevel"]]

        for i in range(len(currentLevel)):
            for j in range(i+1, len(currentLevel)):
                permsToMove.append([i, j])

        for i in range(len(currentLevel)):
            permsToMove.append([i])

        return permsToMove

    def clone(s):
        cl = {
            "floors": [fl[:] for fl in s["floors"]],
            "elevatorLevel": s["elevatorLevel"],
            "steps": s["steps"]
        }
        return cl

    def getNextStates(s):
        futureStates = []
        movablePermIndices = getMovablePermIndices(s)
        eleDiffs = [1] if s["elevatorLevel"] < 3 else []
        eleDiffs += [-1] if s["elevatorLevel"] > 0 else []

        for eleDiff in eleDiffs:
            for permIndices in movablePermIndices:
                cl = clone(s)
                cl["elevatorLevel"] += eleDiff
                cl["steps"] += 1
                oldLevel = s["elevatorLevel"]
                newLevel = cl["elevatorLevel"]

                for index in permIndices:
                    cl["floors"][newLevel].append(s["floors"][oldLevel][index])

                for in_ in range(len(permIndices)-1, -1, -1):
                    cl["floors"][oldLevel][permIndices[in_]] = cl["floors"][oldLevel][-1]
                    cl["floors"][oldLevel] = cl["floors"][oldLevel][:-1]

                if isValid(cl):
                    futureStates.append(cl)

        return futureStates

    currentState = newInitialState(input_str)

    if part == 2:
        currentState["floors"][0] += [
            {"isChip": False, "material": "elerium"},
            {"isChip": True, "material": "elerium"},
            {"isChip": False, "material": "dilithium"},
            {"isChip": True, "material": "dilithium"}
        ]

    queue = [currentState]
    prevStates = {}
    while len(queue) > 0:
        front = queue[0]
        queue = queue[1:]

        if isDone(front):
            return front["steps"]

        hash_key = hashKey(front)
        if hash_key in prevStates:
            continue
        prevStates[hash_key] = True

        nextStates = getNextStates(front)
        queue += nextStates

    return -1

with open("input.txt", "r") as file:
    input_data = file.read()

part = 1
ans = rtgHellDay(input_data, part)
print(ans)
