import strutils, sequtils, math

type Particle = object
  pos, vel, acc: array[3, int]

proc parseParticle(line: string): Particle =
  var parts = line.split(", ")
  var pos = parts[0][3..^2].split(",").mapIt(parseInt(it))
  var vel = parts[1][3..^2].split(",").mapIt(parseInt(it))
  var acc = parts[2][3..^2].split(",").mapIt(parseInt(it))
  Particle(pos: [pos[0], pos[1], pos[2]], vel: [vel[0], vel[1], vel[2]], acc: [acc[0], acc[1], acc[2]])

proc manhattanDistance(p: Particle): int =
  abs(p.pos[0]) + abs(p.pos[1]) + abs(p.pos[2])

proc main =
  let file = readFile("input.txt")
  var particles = file.splitLines().mapIt(parseParticle(it))

  var minAcc = high(int)
  var closestIdx = -1
  for i, p in particles:
    let accSum = abs(p.acc[0]) + abs(p.acc[1]) + abs(p.acc[2])
    if accSum < minAcc:
      minAcc = accSum
      closestIdx = i
    elif accSum == minAcc:
      let velSum = abs(p.vel[0]) + abs(p.vel[1]) + abs(p.vel[2])
      let closestVelSum = abs(particles[closestIdx].vel[0]) + abs(particles[closestIdx].vel[1]) + abs(particles[closestIdx].vel[2])
      if velSum < closestVelSum:
        closestIdx = i

  echo closestIdx

when isMainModule:
  main()