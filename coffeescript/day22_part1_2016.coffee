fs = require 'fs'

class Node
  constructor: (@used, @avail) ->

readNodes = (filename) ->
  data = fs.readFileSync filename, 'utf8'
  nodeRegex = /node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%/
  nodes = []
  for line in data.split '\n'
    matches = nodeRegex.exec line
    if matches
      used = parseInt matches[1]
      avail = parseInt matches[2]
      nodes.push new Node used, avail
  nodes

countViablePairs = (nodes) ->
  count = 0
  for i in [0...nodes.length]
    for j in [0...nodes.length]
      if i != j and nodes[i].used > 0 and nodes[i].used <= nodes[j].avail
        count++
  count

nodes = readNodes 'input.txt'
viablePairs = countViablePairs nodes
console.log viablePairs