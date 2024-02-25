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

# Main program to calculate the number of unique houses that receive at least one present
proc main(filename: string) =
  var
    santaPos: Position = (0, 0)
    roboSantaPos: Position = (0, 0)
    visited: HashSet[Position] = initHashSet[Position]()
  visited.incl(santaPos)

  let directions = readFile(filename)

  # Iterate over the directions, taking turns for Santa and Robo-Santa
  for i, dir in directions:
    if i mod 2 == 0:
      move(santaPos, toMovement(dir))
      visited.incl(santaPos)
    else:
      move(roboSantaPos, toMovement(dir))
      visited.incl(roboSantaPos)

  echo "Houses that received at least one present: ", visited.len

main("input.txt")
