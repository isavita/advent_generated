fs = require 'fs'

class Node
  constructor: (@name) ->
    @children = []
    @parent = null

findOrCreateNode = (name, nodes) ->
  return nodes[name] if nodes[name]
  nodes[name] = new Node(name)

buildOrbitMap = (lines) ->
  nodes = {}
  for line in lines
    parts = line.split(')')
    center = findOrCreateNode(parts[0], nodes)
    orbiter = findOrCreateNode(parts[1], nodes)
    center.children.push orbiter
    orbiter.parent = center
  nodes

pathToRoot = (node) ->
  path = []
  while node
    path.push node
    node = node.parent
  path

findCommonAncestor = (node1, node2) ->
  path1 = pathToRoot(node1)
  path2 = pathToRoot(node2)
  i = path1.length - 1
  j = path2.length - 1
  i-- and j-- while i >= 0 and j >= 0 and path1[i] == path2[j]
  [i + 1, j + 1]

data = fs.readFileSync('input.txt', 'utf8')
lines = data.trim().split('\n')
orbitMap = buildOrbitMap(lines)

[transfersYOU, transfersSAN] = findCommonAncestor(orbitMap['YOU'].parent, orbitMap['SAN'].parent)
console.log transfersYOU + transfersSAN