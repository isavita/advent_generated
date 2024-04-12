fs = require 'fs'

class Vec3
  constructor: (@x, @y, @z) ->

class Moon
  constructor: (@pos, @vel) ->

abs = (x) -> if x < 0 then -x else x

applyGravity = (moons) ->
  for i in [0...moons.length]
    for j in [i+1...moons.length]
      if moons[i].pos.x > moons[j].pos.x
        moons[i].vel.x -= 1
        moons[j].vel.x += 1
      else if moons[i].pos.x < moons[j].pos.x
        moons[i].vel.x += 1
        moons[j].vel.x -= 1

      if moons[i].pos.y > moons[j].pos.y
        moons[i].vel.y -= 1
        moons[j].vel.y += 1
      else if moons[i].pos.y < moons[j].pos.y
        moons[i].vel.y += 1
        moons[j].vel.y -= 1

      if moons[i].pos.z > moons[j].pos.z
        moons[i].vel.z -= 1
        moons[j].vel.z += 1
      else if moons[i].pos.z < moons[j].pos.z
        moons[i].vel.z += 1
        moons[j].vel.z -= 1

applyVelocity = (moons) ->
  for moon in moons
    moon.pos.x += moon.vel.x
    moon.pos.y += moon.vel.y
    moon.pos.z += moon.vel.z

totalEnergy = (moons) ->
  total = 0
  for moon in moons
    pot = abs(moon.pos.x) + abs(moon.pos.y) + abs(moon.pos.z)
    kin = abs(moon.vel.x) + abs(moon.vel.y) + abs(moon.vel.z)
    total += pot * kin
  total

data = fs.readFileSync 'input.txt', 'utf8'
lines = data.trim().split '\n'
moons = []

for line in lines
  [x, y, z] = line.match(/-?\d+/g).map (num) -> parseInt num, 10
  moons.push new Moon(new Vec3(x, y, z), new Vec3(0, 0, 0))

for step in [0...1000]
  applyGravity moons
  applyVelocity moons

console.log totalEnergy moons