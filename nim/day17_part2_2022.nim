
import std/sets
import std/tables
import std/strutils
import std/options
import std/math
import std/os

const ChamberWidth = 7
const ProfileDepth = 30 # Depth for cycle detection profile

type
  Pos = tuple[x, y: int64] # Use int64 for y due to large potential height
  RockShape = seq[Pos]
  Chamber = HashSet[Pos]
  # State for cycle detection: (rock index mod 5, jet index mod len, chamber top profile)
  # Profile is represented as height difference from highestY for each column, capped by ProfileDepth
  Profile = array[ChamberWidth, int]
  State = tuple[rockIdxMod: int, jetIdxMod: int, profile: Profile]

const rockShapes: array[5, RockShape] = [
  # Relative coordinates (dx, dy)
  # Shape 0: -
  @[ (x:0'i64, y:0'i64), (x:1'i64, y:0'i64), (x:2'i64, y:0'i64), (x:3'i64, y:0'i64) ],
  # Shape 1: +
  @[ (x:1'i64, y:0'i64), (x:0'i64, y:1'i64), (x:1'i64, y:1'i64), (x:2'i64, y:1'i64), (x:1'i64, y:2'i64) ],
  # Shape 2: L (inverted)
  @[ (x:0'i64, y:0'i64), (x:1'i64, y:0'i64), (x:2'i64, y:0'i64), (x:2'i64, y:1'i64), (x:2'i64, y:2'i64) ],
  # Shape 3: |
  @[ (x:0'i64, y:0'i64), (x:0'i64, y:1'i64), (x:0'i64, y:2'i64), (x:0'i64, y:3'i64) ],
  # Shape 4: Square
  @[ (x:0'i64, y:0'i64), (x:1'i64, y:0'i64), (x:0'i64, y:1'i64), (x:1'i64, y:1'i64) ]
]

proc canMove(rock: RockShape, dx: int64, dy: int64, chamber: Chamber): Option[RockShape] =
  var movedRock = newSeq[Pos](rock.len)
  for i, p in rock:
    let newX = p.x + dx
    let newY = p.y + dy

    # Check boundaries
    if newX < 0 or newX >= ChamberWidth or newY < 1:
      return none(RockShape)
    # Check collision with settled rocks
    if chamber.contains((x: newX, y: newY)):
      return none(RockShape)
    
    movedRock[i] = (x: newX, y: newY)
  
  return some(movedRock)

proc getChamberProfile(chamber: Chamber, highestY: int64): Profile =
  var profile: Profile
  for x in 0..<ChamberWidth:
    profile[x] = ProfileDepth + 1 # Sentinel value if no rock found in depth range
    let startY = highestY
    let endY = max(1'i64, highestY - ProfileDepth + 1)
    # Search downwards from highestY
    for y in countdown(startY, endY):
        if chamber.contains((x: int64(x), y: y)):
            profile[x] = int(highestY - y) # Store relative depth
            break
  return profile

proc simulate(jetPattern: string, totalRocks: int64): int64 =
  var chamber = initHashSet[Pos]()
  # Initialize floor at y = 0
  for x in 0..<ChamberWidth:
    chamber.incl((x: int64(x), y: 0'i64))

  var highestY: int64 = 0
  let jetLen = jetPattern.len
  var jetIndex: int64 = 0
  var rockIndex: int = 0

  # For cycle detection
  var seenStates = initTable[State, tuple[rockNum: int64, height: int64]]()
  var additionalHeight: int64 = 0
  var rockNumber: int64 = 0

  while rockNumber < totalRocks:
    let shape = rockShapes[rockIndex mod 5]
    # Initial position: left edge 2 units from left wall, bottom 3 units above highest point
    var currentRockPos = (x: 2'i64, y: highestY + 4'i64)
    var currentRock = newSeq[Pos](shape.len)
    for i, offset in shape:
        currentRock[i] = (x: currentRockPos.x + offset.x, y: currentRockPos.y + offset.y)

    while true:
      # 1. Apply jet push
      let jetDir = jetPattern[int(jetIndex mod int64(jetLen))]
      jetIndex += 1
      var dx: int64 = 0
      if jetDir == '>': dx = 1
      elif jetDir == '<': dx = -1
      
      let movedLateral = canMove(currentRock, dx, 0'i64, chamber)
      if movedLateral.isSome:
        currentRock = movedLateral.get()

      # 2. Attempt to move down
      let movedDown = canMove(currentRock, 0'i64, -1'i64, chamber)
      if movedDown.isSome:
        currentRock = movedDown.get()
      else:
        # Rock comes to rest
        for pos in currentRock:
          chamber.incl(pos)
          highestY = max(highestY, pos.y)
        break # Move to the next rock

    # Cycle detection part
    let profile = getChamberProfile(chamber, highestY)
    let currentState: State = (rockIdxMod: rockIndex mod 5, 
                               jetIdxMod: int(jetIndex mod int64(jetLen)), 
                               profile: profile)

    if currentState in seenStates:
        let (prevRockNum, prevHeight) = seenStates[currentState]
        let cycleLenRocks = rockNumber - prevRockNum
        let cycleHeightGain = highestY - prevHeight

        let remainingRocks = totalRocks - rockNumber - 1 # -1 because current rock is done
        if remainingRocks > 0 and cycleLenRocks > 0:
            let numCycles = remainingRocks div cycleLenRocks
            
            additionalHeight += numCycles * cycleHeightGain
            rockNumber += numCycles * cycleLenRocks
            # Clear seen states to prevent re-detecting the same cycle if patterns shift subtly
            # after the jump. Alternatively, could just let it run, but clearing is safer.
            seenStates = initTable[State, tuple[rockNum: int64, height: int64]]() 

    else:
        seenStates[currentState] = (rockNum: rockNumber, height: highestY)

    rockNumber += 1
    rockIndex += 1
    
  return highestY + additionalHeight

proc main() =
  let filename = "input.txt"
  if not fileExists(filename):
    echo "Error: Input file '", filename, "' not found."
    quit(1)
  
  let jetPattern = readFile(filename).strip()
  if jetPattern.len == 0:
      echo "Error: Input file is empty or contains only whitespace."
      quit(1)

  let totalRocks = 1_000_000_000_000'i64
  let finalHeight = simulate(jetPattern, totalRocks)
  echo finalHeight

main()
