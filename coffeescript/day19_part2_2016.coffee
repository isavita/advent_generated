fs = require 'fs'

class LLNode
  constructor: (@elfNum, @presents = 1, @next = null) ->

elephant = (input) ->
  startingElves = parseInt input
  root = new LLNode 1
  iter = root
  i = 2
  while i <= startingElves
    iter.next = new LLNode i
    iter = iter.next
    i++
  iter.next = root

  isOddLength = startingElves % 2 == 1
  beforeAcross = root
  i = 0
  while i < Math.floor(startingElves / 2) - 1
    beforeAcross = beforeAcross.next
    i++

  while root.next != root
    root.presents += beforeAcross.next.presents
    beforeAcross.next = beforeAcross.next.next
    if isOddLength
      beforeAcross = beforeAcross.next
    isOddLength = !isOddLength
    root = root.next

  return root.elfNum

readFile = (pathFromCaller) ->
  content = fs.readFileSync pathFromCaller, encoding: 'utf8'
  content.trim()

input = readFile './input.txt'
ans = elephant input
console.log ans