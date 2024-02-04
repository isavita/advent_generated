
with open("input.txt", "r") as file:
    lines = file.readlines()

time = int(lines[0].split(":")[1].replace(" ", ""))
distance = int(lines[1].split(":")[1].replace(" ", ""))

def calculateWaysToWinLongRace(time, record):
    waysToWin = 0
    for holdTime in range(1, time):
        travelTime = time - holdTime
        distance = holdTime * travelTime
        if distance > record:
            waysToWin += 1
    return waysToWin

waysToWin = calculateWaysToWinLongRace(time, distance)

print(waysToWin)
