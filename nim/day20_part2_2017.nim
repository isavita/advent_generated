
import strutils, tables, sets, algorithm, sequtils

const InputFile = "input.txt"
const SimSteps = 1000

type
  Vec3 = array[3, int]
  Particle = object
    p, v, a: Vec3

proc main() =
  var particles: seq[Particle] = @[]

  # Read and parse input file
  for line in readFile(InputFile).strip.splitLines:
    if line.len == 0: continue

    # Basic parsing assuming fixed format "p=<x,y,z>, v=<x,y,z>, a=<x,y,z>"
    var components: array[3, Vec3] # 0=p, 1=v, 2=a
    var partIndex = 0
    var currentComponent = 0
    var numStr = ""

    for i, c in line:
      if c == '<':
        currentComponent = 0
        numStr = ""
      elif c.isDigit or c == '-':
        numStr.add(c)
      elif (c == ',' or c == '>') and numStr.len > 0:
        components[partIndex][currentComponent] = parseInt(numStr)
        numStr = ""
        currentComponent += 1
        if c == '>':
          partIndex += 1
      # Skip other characters like 'p', '=', ' ', 'v', 'a'

    particles.add Particle(p: components[0], v: components[1], a: components[2])

  # Simulation Loop
  for _ in 0 ..< SimSteps:
    if particles.len == 0: break # Optimization

    # Update particles
    for i in 0 ..< particles.len:
      for j in 0 .. 2:
        particles[i].v[j] += particles[i].a[j]
        particles[i].p[j] += particles[i].v[j]

    # Collision Detection
    var positions = initTable[Vec3, seq[int]]()
    for i in 0 ..< particles.len:
      let pos = particles[i].p
      positions.mgetOrPut(pos, @[]).add(i) # Efficiently add index

    # Identify indices to remove (using a set for uniqueness)
    var indicesToRemove = initHashSet[int]()
    for posIndices in positions.values:
      if posIndices.len > 1:
        for idx in posIndices:
          indicesToRemove.incl(idx)

    # Remove collided particles (optimized in-place removal)
    if indicesToRemove.len > 0:
      # Sort indices descending to avoid shifting issues during deletion
      var sortedIndices = toSeq(indicesToRemove.items).sorted(SortOrder.Descending)
      for idx in sortedIndices:
        particles.delete(idx)

  # Print result
  echo particles.len

main()
