
fs = require 'fs'

trim = (s) -> s.replace(/^\s+|\s+$/g, '')

lines = fs.readFileSync('input.txt', 'utf8').split /\r?\n/

ranges = []
parsing = true
fresh = 0

for line in lines
  s = trim line
  if s == ''
    if parsing
      parsing = false
      if ranges.length
        ranges.sort (a, b) ->
          if a.min != b.min then a.min - b.min else a.max - b.max
        merged = []
        for rg in ranges
          if merged.length == 0 or rg.min > merged[merged.length - 1].max
            merged.push rg
          else if rg.max > merged[merged.length - 1].max
            merged[merged.length - 1].max = rg.max
        ranges = merged
    continue

  if parsing
    parts = s.split '-'
    continue unless parts.length == 2
    mn = parseInt parts[0].trim()
    mx = parseInt parts[1].trim()
    ranges.push min: mn, max: mx
  else
    id = parseInt s
    continue if isNaN id
    l = 0
    r = ranges.length
    found = false
    while l < r
      m = (l + r) >>> 1
      rg = ranges[m]
      if id < rg.min
        r = m
      else if id > rg.max
        l = m + 1
      else
        found = true
        break
    fresh++ if found

console.log "Number of fresh ingredients: #{fresh}"
