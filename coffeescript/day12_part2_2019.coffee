
fs = require 'fs'

lcm = (a, b) ->
  bigA = BigInt(a)
  bigB = BigInt(b)
  bigA * bigB / (gcd(bigA, bigB))

gcd = (a, b) ->
  while b != 0n
    [a, b] = [b, a % b]
  a

applyGravity = (moons, axis) ->
  for i in [0...moons.length]
    for j in [i + 1...moons.length]
      if moons[i].pos[axis] > moons[j].pos[axis]
        moons[i].vel[axis]--
        moons[j].vel[axis]++
      else if moons[i].pos[axis] < moons[j].pos[axis]
        moons[i].vel[axis]++
        moons[j].vel[axis]--

applyVelocity = (moons, axis) ->
  for moon in moons
    moon.pos[axis] += moon.vel[axis]

findCycle = (moons, initialMoons, axis) ->
  steps = 0
  while true
    steps++
    applyGravity(moons, axis)
    applyVelocity(moons, axis)
    match = true
    for i in [0...moons.length]
      if moons[i].pos[axis] != initialMoons[i].pos[axis] or moons[i].vel[axis] != initialMoons[i].vel[axis]
        match = false
        break
    if match
      return steps

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
moons = []
initialMoons = []

for line in input
  [x, y, z] = line.match(/<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/).slice(1).map(Number)
  moons.push({pos: {x, y, z}, vel: {x: 0, y: 0, z: 0}})
  initialMoons.push({pos: {x, y, z}, vel: {x: 0, y: 0, z: 0}})

cycleX = findCycle(JSON.parse(JSON.stringify(moons)), initialMoons, 'x')
cycleY = findCycle(JSON.parse(JSON.stringify(moons)), initialMoons, 'y')
cycleZ = findCycle(JSON.parse(JSON.stringify(moons)), initialMoons, 'z')

lcmXY = lcm(cycleX, cycleY)
lcmXYZ = lcm(lcmXY, cycleZ)

console.log(lcmXYZ.toString())
