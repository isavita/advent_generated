
fs = require 'fs'

graph = {}
for line in fs.readFileSync('input.txt','utf8').trim().split('\n')
    [from, rest] = line.split ': '
    for to in rest.split ' '
        (graph[from] ?= []).push to

memo = {}
dfs = (node) ->
    return 1 if node is 'out'
    return memo[node] if memo[node]?
    total = 0
    for neighbor in graph[node] ? []
        total += dfs neighbor
    memo[node] = total
    total

console.log dfs 'you'
