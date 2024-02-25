
import strutils

var inputFile = open("input.txt")
var lines = inputFile.readAll.split('\n')

var count = 0

for line in lines:
    var pair = line.split(',')
    var pair1 = pair[0].split('-')
    var pair2 = pair[1].split('-')

    var start1 = pair1[0].parseInt
    var end1 = pair1[1].parseInt
    var start2 = pair2[0].parseInt
    var end2 = pair2[1].parseInt

    if (start1 <= start2 and end1 >= end2) or (start2 <= start1 and end2 >= end1):
        count += 1

echo count
