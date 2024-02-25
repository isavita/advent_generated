
import os
import strutils

var file = open("input.txt")
var lines = file.readAll.splitLines

var lanternFishCounts: array[9, int]

for line in lines:
  var fishAges = line.split(",")
  for age in fishAges:
    lanternFishCounts[parseInt(age)] += 1

for i in 0 ..< 256:
  var newLanternFish = lanternFishCounts[0]
  for j in 0 ..< lanternFishCounts.high:
    lanternFishCounts[j] = lanternFishCounts[j + 1]
  lanternFishCounts[6] += newLanternFish
  lanternFishCounts[8] = newLanternFish

var totalFishCount = 0
for count in lanternFishCounts:
  totalFishCount += count

echo totalFishCount
