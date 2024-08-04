fs = require 'fs'

class Particle
  constructor: (@p, @v, @a) ->

file = fs.readFileSync 'input.txt', 'utf8'
lines = file.split '\n'
particles = []

for line in lines
  [p, v, a] = line.match(/<([^>]+)>/g).map (s) -> s.slice(1, -1).split(',').map Number
  particles.push new Particle p, v, a

for tick in [0...1000]
  positions = {}
  for particle, i in particles
    for j in [0...3]
      particle.v[j] += particle.a[j]
      particle.p[j] += particle.v[j]
    posStr = particle.p.join ','
    positions[posStr] ?= 0
    positions[posStr]++

  particles = (p for p in particles when positions[p.p.join ','] == 1)

console.log particles.length