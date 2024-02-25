
import strutils

var file = open("input.txt")
var lines = file.readAll.splitLines

for i in 0..<len(lines)-1:
    for j in i+1..<len(lines):
        var diff = 0
        for k in 0..<len(lines[i]):
            if lines[i][k] != lines[j][k]:
                diff += 1
                if diff > 1:
                    break
        if diff == 1:
            var common = ""
            for k in 0..<len(lines[i]):
                if lines[i][k] == lines[j][k]:
                    common.add(lines[i][k])
            echo common
            quit()
