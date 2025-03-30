
import sequtils, strutils, tables, algorithm, strformat, sets

type
  Coord = tuple[x: int, y: int]
  TurnDirection = enum
    tdLeft, tdStraight, tdRight
  Cart = object
    id: int
    pos: Coord
    dir: char
    nextTurn: TurnDirection
    crashed: bool

proc parseInput(filePath: string): (Table[Coord, char], seq[Cart]) =
  var tracks: Table[Coord, char]
  var carts: seq[Cart]
  var cartIdCounter = 0
  var y = 0
  for line in lines(filePath):
    for x, c in line:
      let currentPos: Coord = (x: x, y: y)
      case c
      of '^', 'v', '<', '>':
        let trackChar = if c in {'^', 'v'}: '|' else: '-'
        tracks[currentPos] = trackChar
        carts.add(Cart(id: cartIdCounter, pos: currentPos, dir: c, nextTurn: tdLeft, crashed: false))
        inc cartIdCounter
      of '|', '-', '+', '/', '\\':
        tracks[currentPos] = c
      else:
        discard # Ignore empty space
    inc y
  result = (tracks, carts)

proc moveCart(cart: var Cart, tracks: Table[Coord, char]) =
  var dx, dy: int
  case cart.dir
  of '^': dy = -1
  of 'v': dy = 1
  of '<': dx = -1
  of '>': dx = 1
  else: discard

  let newPos: Coord = (x: cart.pos.x + dx, y: cart.pos.y + dy)
  cart.pos = newPos

  let track = tracks.getOrDefault(newPos)

  case track
  of '/':
    case cart.dir
    of '^': cart.dir = '>'
    of 'v': cart.dir = '<'
    of '<': cart.dir = 'v'
    of '>': cart.dir = '^'
    else: discard
  of '\\':
    case cart.dir
    of '^': cart.dir = '<'
    of 'v': cart.dir = '>'
    of '<': cart.dir = '^'
    of '>': cart.dir = 'v'
    else: discard
  of '+':
    case cart.nextTurn
    of tdLeft:
      case cart.dir
      of '^': cart.dir = '<'
      of 'v': cart.dir = '>'
      of '<': cart.dir = 'v'
      of '>': cart.dir = '^'
      else: discard
      cart.nextTurn = tdStraight
    of tdStraight:
      # Direction remains the same
      cart.nextTurn = tdRight
    of tdRight:
      case cart.dir
      of '^': cart.dir = '>'
      of 'v': cart.dir = '<'
      of '<': cart.dir = '^'
      of '>': cart.dir = 'v'
      else: discard
      cart.nextTurn = tdLeft
  else: # '-', '|'
    discard # Direction remains the same

proc simulate(tracks: Table[Coord, char], initialCarts: seq[Cart]): Coord =
  var carts = initialCarts
  var occupied = initHashSet[Coord]()
  var crashedIds = initHashSet[int]()

  while carts.len > 1:
    carts.sort(proc(a, b: Cart): int = cmp((a.pos.y, a.pos.x), (b.pos.y, b.pos.x)))
    occupied.clear()
    crashedIds.clear()

    # Pre-populate occupied set for initial collision check during moves
    for cart in carts:
      occupied.incl(cart.pos)

    for i in 0 ..< carts.len:
      if carts[i].crashed: continue # Already crashed this tick or previously

      let oldPos = carts[i].pos
      occupied.excl(oldPos) # Cart is moving away

      moveCart(carts[i], tracks)

      let newPos = carts[i].pos

      # Check for immediate collision after move
      if occupied.contains(newPos):
          carts[i].crashed = true
          crashedIds.incl(carts[i].id)
          # Find the other cart(s) at the collision site and mark them
          for j in 0 ..< carts.len:
              if i != j and carts[j].pos == newPos and not carts[j].crashed:
                  carts[j].crashed = true
                  crashedIds.incl(carts[j].id)
          occupied.excl(newPos) # Remove the collision spot as carts are removed
      else:
          occupied.incl(newPos) # Mark the new position as occupied

    # Filter out crashed carts efficiently
    var nextCarts: seq[Cart] = @[]
    for cart in carts:
        if not cart.crashed:
            nextCarts.add(cart)
    carts = nextCarts

  if carts.len == 1:
    return carts[0].pos
  else:
    # Should not happen based on problem description if input is valid
    raise newException(ValueError, "Simulation ended with zero carts")


proc main() =
  let (tracks, carts) = parseInput("input.txt")
  let lastCartPos = simulate(tracks, carts)
  echo fmt"The last cart is at: {lastCartPos.x},{lastCartPos.y}"

when isMainModule:
  main()
