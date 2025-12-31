
fs = require 'fs'

lines = fs.readFileSync('input.txt','utf8').split /\r?\n/
id = new Map()
adj = []

getId = (s) ->
  unless id.has s
    idx = id.size
    id.set s, idx
    adj.push []
  id.get s

trim = (s) -> s.replace /^\s+|\s+$/g, ''

for line in lines
  continue unless (p = line.indexOf ':') >= 0
  src = trim line[0...p]
  continue unless src
  dst = trim line[p+1..]
  u = getId src
  for token in dst.split /\s+/ when token.length
    v = getId token
    adj[u].push v

svr = getId 'svr'
dac = getId 'dac'
fft = getId 'fft'
out = getId 'out'

dfs = (cur, target, memo) ->
  return 1 if cur is target
  return memo[cur] if memo[cur] isnt -1
  sum = 0
  for v in adj[cur]
    sum += dfs v, target, memo
  memo[cur] = sum
  sum

countPaths = (s, t) ->
  memo = ( -1 for _ in [0...adj.length] )
  dfs s, t, memo

s1 = countPaths(svr,dac) * countPaths(dac,fft) * countPaths(fft,out)
s2 = countPaths(svr,fft) * countPaths(fft,dac) * countPaths(dac,out)

console.log "Paths (svr->dac->fft->out): #{s1}"
console.log "Paths (svr->fft->dac->out): #{s2}"
console.log "Total paths visiting both: #{s1+s2}"
