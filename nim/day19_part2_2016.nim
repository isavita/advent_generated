
import os
import strutils

type LLNode = ref object
    elfNum: int
    presents: int
    next: LLNode

proc elephant(input: string): int =
    var startingElves = parseInt(input)
    var root: LLNode = new(LLNode)
    root.elfNum = 1
    root.presents = 1
    var iter = root
    for i in 2..startingElves:
        iter.next = new(LLNode)
        iter.next.elfNum = i
        iter.next.presents = 1
        iter = iter.next
    iter.next = root

    var isOddLength = startingElves mod 2 == 1
    var beforeAcross = root
    for i in 0..<startingElves div 2 - 1:
        beforeAcross = beforeAcross.next

    while root.next != root:
        root.presents += beforeAcross.next.presents
        beforeAcross.next = beforeAcross.next.next

        if isOddLength:
            beforeAcross = beforeAcross.next
        isOddLength = not isOddLength
        root = root.next

    return root.elfNum

var file = open("input.txt")
var input = file.readLine().strip()
file.close()

echo elephant(input)
