
import strutils, math, sequtils

type Vec3 = array[3, int]

proc updateVelocity(moons: seq[Vec3], velocities: var seq[Vec3]) =
  let n = moons.len
  for i in 0 ..< n:
    for j in i + 1 ..< n:
      for axis in 0 .. 2:
        if moons[i][axis] < moons[j][axis]:
          velocities[i][axis] += 1
          velocities[j][axis] -= 1
        elif moons[i][axis] > moons[j][axis]:
          velocities[i][axis] -= 1
          velocities[j][axis] += 1

proc updatePositions(moons: var seq[Vec3], velocities: seq[Vec3]) =
  let n = moons.len
  for i in 0 ..< n:
    for axis in 0 .. 2:
      moons[i][axis] += velocities[i][axis]

proc calculateEnergy(moons, velocities: seq[Vec3]): int =
  var totalEnergy = 0
  for i in 0 ..< moons.len:
    var potential = 0
    var kinetic = 0
    for axis in 0 .. 2:
      potential += abs(moons[i][axis])
      kinetic += abs(velocities[i][axis])
    totalEnergy += potential * kinetic
  return totalEnergy

proc main() =
  var moons: seq[Vec3] = @[]
  for line in readFile("input.txt").splitLines:
    if line.len > 0:
      let parts = line[1 .. ^2].split(", ") # Remove <>, split by ", "
      var pos: Vec3
      for i, part in parts:
          pos[i] = part.split("=")[1].parseInt
      moons.add(pos)

  var velocities = newSeq[Vec3](moons.len) # Initializes with zeros

  for _ in 1..1000:
    updateVelocity(moons, velocities)
    updatePositions(moons, velocities)

  echo calculateEnergy(moons, velocities)

when isMainModule:
  main()
