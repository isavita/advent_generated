
import os
import sys
import string

hashTableSize = 256

class Step:
    def __init__(self, label, numBox, operation, number):
        self.label = label
        self.numBox = numBox
        self.operation = operation
        self.number = number

def hashString(s):
    res = 0
    for char in s:
        res += ord(char)
        res *= 17
        res %= hashTableSize
    return res

def parseStep(stepStr):
    label = stepStr.rstrip("=-0123456789")
    numBox = hashString(label)
    operation = stepStr[len(label):len(label)+1]
    number = None
    if operation == "=":
        number = int(stepStr[len(label)+1:])
    return Step(label, numBox, operation, number)

def getBoxes(stepsStr):
    boxes = {}
    for stepStr in stepsStr:
        step = parseStep(stepStr)
        boxContents = boxes.get(step.numBox, [])

        if step.operation == "-":
            for i, content in enumerate(boxContents):
                if step.label in content:
                    del boxContents[i]
                    break
        elif step.operation == "=":
            found = False
            for content in boxContents:
                if step.label in content:
                    content[step.label] = step.number
                    found = True
                    break
            if not found:
                boxContents.append({step.label: step.number})

        if len(boxContents) == 0:
            boxes.pop(step.numBox, None)
        else:
            boxes[step.numBox] = boxContents

    return boxes

def toStringBoxes(boxes):
    res = ""
    for iBox in range(hashTableSize):
        if iBox in boxes:
            res += f"Box {iBox} : "
            for content in boxes[iBox]:
                for key, value in content.items():
                    res += f"[{key} {value}] "
            res += "\n"
    return res

def calculatePower(boxes):
    res = 0
    for iBox in range(hashTableSize):
        if iBox in boxes:
            for iSlot, content in enumerate(boxes[iBox]):
                for value in content.values():
                    res += (iBox + 1) * (iSlot + 1) * value
    return res

def solve(input):
    line = input[0]
    stepsStr = line.split(",")

    boxes = getBoxes(stepsStr)

    return calculatePower(boxes)

def readFile(fileName):
    with open(fileName, 'r') as file:
        return file.read().strip().split("\n")

def main():
    input = readFile("input.txt")
    print(solve(input))

if __name__ == "__main__":
    main()
