import sets

type
  Position = tuple[x, y: int]
  Movement = enum
    North, South, East, West

# Converts a character to a Movement
proc toMovement(c: char): Movement =
  case c
  of '^': return Movement.North
  of 'v': return Movement.South
  of '>': return Movement.East
  of '<': return Movement.West
  else: raise newException(ValueError, "Invalid movement character")

# Updates the position based on the movement
proc move(pos: var Position, mov: Movement) =
  case mov
  of Movement.North: inc(pos.y)
  of Movement.South: dec(pos.y)
  of Movement.East: inc(pos.x)
  of Movement.West: dec(pos.x)

proc main(filename: string) =
  var
    santaPos: Position = (0, 0) # Starting position
    visited: HashSet[Position] = initHashSet[Position]()
  visited.incl(santaPos) # Santa delivers a present to the starting house

  let directions = readFile(filename)

  for dir in directions:
    move(santaPos, toMovement(dir))
    visited.incl(santaPos) 

  echo "Houses that received at least one present: ", visited.len

main("input.txt")
