
fs = require 'fs'

graph = {}

addConnection = (graph, from, to) ->
  graph[from] ?= {}
  graph[from][to] = true

lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

for line in lines
  computers = line.split('-')
  if computers.length is 2
    addConnection graph, computers[0], computers[1]
    addConnection graph, computers[1], computers[0]

findTripletsWithT = (graph) ->
  triplets = []
  seen = {}
  computers = Object.keys graph

  for i in [0...computers.length]
    for j in [i + 1...computers.length]
      for k in [j + 1...computers.length]
        c1 = computers[i]
        c2 = computers[j]
        c3 = computers[k]
        if graph[c1]? and graph[c1][c2] and graph[c2]? and graph[c2][c3] and graph[c1][c3]
          if c1.startsWith('t') or c2.startsWith('t') or c3.startsWith('t')
            triplet = [c1, c2, c3].sort()
            key = triplet.join(',')
            unless seen[key]
              triplets.push triplet
              seen[key] = true
  triplets

tripletsWithT = findTripletsWithT graph
console.log "Number of triplets containing at least one computer with name starting with 't':", tripletsWithT.length
