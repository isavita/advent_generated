
import strutils, parseutils, math, sequtils, sets

# --- Type Definitions ---

type
  Vec3* = object
    x*, y*, z*: int

  Moon* = object
    pos*, vel*: Vec3

# --- Utility Functions ---

func `$`(v: Vec3): string =
  result = "<x=" & $v.x & ", y=" & $v.y & ", z=" & $v.z & ">"

func `$`(m: Moon): string =
  result = "pos=" & $m.pos & ", vel=" & $m.vel

# Overload addition for Vec3
func `+`*(a, b: Vec3): Vec3 =
  result = Vec3(x: a.x + b.x, y: a.y + b.y, z: a.z + b.z)

# Overload addition assignment for Vec3
proc `+=`*(a: var Vec3, b: Vec3) =
  a.x += b.x
  a.y += b.y
  a.z += b.z

# Calculate energy for a single moon
func energy(m: Moon): int =
  let pot = abs(m.pos.x) + abs(m.pos.y) + abs(m.pos.z)
  let kin = abs(m.vel.x) + abs(m.vel.y) + abs(m.vel.z)
  return pot * kin

# Calculate total energy for the system
func totalEnergy(moons: seq[Moon]): int =
  for m in moons:
    result += m.energy()

# --- Simulation Logic ---

# Apply gravity updates between two moons for all axes
proc applyGravityPair(m1: var Moon, m2: var Moon) =
  # X axis
  if m1.pos.x < m2.pos.x:
    m1.vel.x += 1
    m2.vel.x -= 1
  elif m1.pos.x > m2.pos.x:
    m1.vel.x -= 1
    m2.vel.x += 1

  # Y axis
  if m1.pos.y < m2.pos.y:
    m1.vel.y += 1
    m2.vel.y -= 1
  elif m1.pos.y > m2.pos.y:
    m1.vel.y -= 1
    m2.vel.y += 1

  # Z axis
  if m1.pos.z < m2.pos.z:
    m1.vel.z += 1
    m2.vel.z -= 1
  elif m1.pos.z > m2.pos.z:
    m1.vel.z -= 1
    m2.vel.z += 1

# Apply velocity updates to all moons
proc applyVelocity(moons: var seq[Moon]) =
  for i in 0 ..< moons.len:
    moons[i].pos += moons[i].vel

# Perform one full simulation step
proc step(moons: var seq[Moon]) =
  # Apply gravity (consider all pairs)
  for i in 0 ..< moons.len:
    for j in i + 1 ..< moons.len:
      applyGravityPair(moons[i], moons[j])

  # Apply velocity
  applyVelocity(moons)

# --- Parsing ---

# Parse a line like "<x=-1, y=0, z=2>" into a Vec3
func parseVec3(line: string): Vec3 =
  let parts = line.strip(chars = {'<', '>'}).split(", ")
  var x, y, z: int
  discard parseInt(parts[0][2..^1], x) # Skip "x="
  discard parseInt(parts[1][2..^1], y) # Skip "y="
  discard parseInt(parts[2][2..^1], z) # Skip "z="
  return Vec3(x: x, y: y, z: z)

# --- Part 2 Utilities ---

# Greatest Common Divisor
func gcd(a, b: int): int =
  var a = abs(a)
  var b = abs(b)
  while b != 0:
    let temp = b
    b = a mod b
    a = temp
  return a

# Least Common Multiple
func lcm(a, b: int): int =
  if a == 0 or b == 0:
    return 0
  # Use floating point division first to avoid potential intermediate overflow
  # then convert back. Ensure result is integer.
  # Nim's `int` is 64-bit, should be large enough for expected results.
  # Direct calculation: (abs(a) * abs(b)) div gcd(a, b)
  result = (abs(a) div gcd(a, b)) * abs(b)


# Get the state of a single axis (positions and velocities)
type AxisState = tuple[pos: seq[int], vel: seq[int]]

func getAxisState(moons: seq[Moon], axis: int): AxisState =
  var positions: seq[int] = @[]
  var velocities: seq[int] = @[]
  for m in moons:
    case axis
    of 0:
      positions.add(m.pos.x)
      velocities.add(m.vel.x)
    of 1:
      positions.add(m.pos.y)
      velocities.add(m.vel.y)
    of 2:
      positions.add(m.pos.z)
      velocities.add(m.vel.z)
    else: discard # Should not happen
  return (positions, velocities)

# Perform one simulation step for a single axis
proc stepAxis(moons: var seq[Moon], axis: int) =
  # Apply gravity for the specified axis
  for i in 0 ..< moons.len:
    for j in i + 1 ..< moons.len:
      var p1, p2: int
      case axis
      of 0: p1 = moons[i].pos.x; p2 = moons[j].pos.x
      of 1: p1 = moons[i].pos.y; p2 = moons[j].pos.y
      of 2: p1 = moons[i].pos.z; p2 = moons[j].pos.z
      else: continue # Should not happen

      var dv = 0
      if p1 < p2: dv = 1
      elif p1 > p2: dv = -1

      if dv != 0:
        case axis
        of 0: moons[i].vel.x += dv; moons[j].vel.x -= dv
        of 1: moons[i].vel.y += dv; moons[j].vel.y -= dv
        of 2: moons[i].vel.z += dv; moons[j].vel.z -= dv
        else: discard

  # Apply velocity for the specified axis
  for i in 0 ..< moons.len:
    case axis
    of 0: moons[i].pos.x += moons[i].vel.x
    of 1: moons[i].pos.y += moons[i].vel.y
    of 2: moons[i].pos.z += moons[i].vel.z
    else: discard


# Find the cycle length for a single axis
func findAxisCycle(initialMoons: seq[Moon], axis: int): int =
  var currentMoons = initialMoons # Work on a copy
  let initialState = getAxisState(currentMoons, axis)
  var steps = 0
  while true:
    stepAxis(currentMoons, axis)
    steps += 1
    if getAxisState(currentMoons, axis) == initialState:
      return steps


# --- Main Program ---

proc main() =
  # --- Read Input ---
  let inputFile = "input.txt"
  var initialMoons: seq[Moon]
  try:
    for line in readFile(inputFile).strip().splitLines():
      if line.len > 0:
        initialMoons.add(Moon(pos: parseVec3(line), vel: Vec3(x:0, y:0, z:0)))
  except IOError:
    echo "Error: Cannot read input file: ", inputFile
    quit(1)

  if initialMoons.len == 0:
    echo "Error: No moons parsed from input file."
    quit(1)

  # --- Part 1 ---
  block part1:
    var moons = initialMoons # Work on a copy for Part 1
    for _ in 1..1000:
      step(moons)
    echo "Part 1: Total energy after 1000 steps = ", totalEnergy(moons)

  # --- Part 2 ---
  block part2:
    # Find cycle length for each axis independently
    let cycleX = findAxisCycle(initialMoons, 0)
    let cycleY = findAxisCycle(initialMoons, 1)
    let cycleZ = findAxisCycle(initialMoons, 2)

    # The total cycle length is the LCM of the individual axis cycles
    let totalCycle = lcm(lcm(cycleX, cycleY), cycleZ)
    echo "Part 2: Steps to return to previous state = ", totalCycle


# --- Entry Point ---
when isMainModule:
  main()
