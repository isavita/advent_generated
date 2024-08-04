fs = require 'fs'

class Network
  constructor: (@instructions, @nodes) ->

parseInput = (input) ->
  instructions = input[0]
  nodes = {}
  for line in input[2..]
    [head, children] = parseLine line
    nodes[head] = children
  new Network instructions, nodes

parseLine = (line) ->
  [head, children] = line.split ' = '
  children = children.slice(1, -1).split ', '
  [head, children]

gcd = (a, b) ->
  while b != 0
    [a, b] = [b, a % b]
  a

lcm = (a, b) ->
  (a * b) / gcd(a, b)

lcmSlice = (nums) ->
  return 0 if nums.length == 0
  res = nums[0]
  for num in nums[1..]
    res = lcm res, num
  res

solve = (input) ->
  network = parseInput input
  starts = []
  for node of network.nodes
    lastIndex = node.length - 1
    starts.push node if node[lastIndex] == 'A'

  steps = new Array(starts.length).fill(0)
  instructionsLength = network.instructions.length
  for element, i in starts
    lastIndex = element.length - 1
    while element[lastIndex] != 'Z'
      instruction = network.instructions[steps[i] % instructionsLength]
      element = if instruction == 'L' then network.nodes[element][0] else network.nodes[element][1]
      steps[i]++

  lcmSlice steps

readFile = (fileName) ->
  fs.readFileSync(fileName, 'utf8').trim().split '\n'

input = readFile 'input.txt'
console.log solve input