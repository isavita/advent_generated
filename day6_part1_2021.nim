
import os
import strutils

var file = open("input.txt")
let input = file.readLine()
file.close()

var fishes: array[9, int]
for fishStr in input.split(","):
    let fish = parseInt(fishStr)
    fishes[fish] += 1

for day in 1..80:
    var newFish = fishes[0]
    for i in 1..8:
        fishes[i-1] = fishes[i]
    fishes[6] += newFish
    fishes[8] = newFish

var totalFish = 0
for fish in fishes:
    totalFish += fish

echo totalFish
