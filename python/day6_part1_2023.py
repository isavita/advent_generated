import os, sys, fileinput
from itertools import product

def calculate_ways(times, distances):
    ways = 1
    for (time, distance) in zip(times, distances):
        ways *= number_of_ways_to_win(time, distance)
    return ways

# Calculate the number of winning scenarios for a single race
def number_of_ways_to_win(time, record):
    ways = 0
    for holdTime in range(1, time + 1):
        travelTime = time - holdTime
        distance = holdTime * travelTime
        if distance > record:
            ways += 1
    return ways

def main():
    with open("input.txt") as input_file, open("output.txt", "w") as output_file:
        times = []
        distances = []
        for line in fileinput.input(files=("input.txt")):
            values = line.strip().split()
            intValues = [int(value) for value in values if value.isdigit()]
            if not len(times):
                times += intValues
            else:
                distances += intValues
        total_ways = calculate_ways(times, distances)
    print("Total ways to win:", total_ways)

if __name__ == "__main__":
    main()