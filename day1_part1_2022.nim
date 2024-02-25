
import os
import strutils

var file = open("input.txt")
var maxCalories = 0
var currentCalories = 0

for line in file.lines:
  if line == "":
    if currentCalories > maxCalories:
      maxCalories = currentCalories
    currentCalories = 0
    continue

  let calories = parseInt(line)
  currentCalories += calories

if currentCalories > maxCalories:
  maxCalories = currentCalories

echo maxCalories
