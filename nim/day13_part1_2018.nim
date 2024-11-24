
import strutils, sequtils, algorithm

type Cart = object
  x, y: int
  dir: char
  turn: int

proc moveCart(track: seq[seq[char]], cart: var Cart): void =
  case cart.dir:
    of '>':
      case track[cart.y][cart.x + 1]:
        of '\\': cart.dir = 'v'
        of '/': cart.dir = '^'
        of '+':
          if cart.turn == 0: cart.dir = '^'; cart.turn = 1
          elif cart.turn == 1: cart.turn = 2
          else: cart.dir = 'v'; cart.turn = 0
        else: discard
      cart.x += 1
    of '<':
      case track[cart.y][cart.x - 1]:
        of '\\': cart.dir = '^'
        of '/': cart.dir = 'v'
        of '+':
          if cart.turn == 0: cart.dir = 'v'; cart.turn = 1
          elif cart.turn == 1: cart.turn = 2
          else: cart.dir = '^'; cart.turn = 0
        else: discard
      cart.x -= 1
    of '^':
      case track[cart.y - 1][cart.x]:
        of '\\': cart.dir = '<'
        of '/': cart.dir = '>'
        of '+':
          if cart.turn == 0: cart.dir = '<'; cart.turn = 1
          elif cart.turn == 1: cart.turn = 2
          else: cart.dir = '>'; cart.turn = 0
        else: discard
      cart.y -= 1
    of 'v':
      case track[cart.y + 1][cart.x]:
        of '\\': cart.dir = '>'
        of '/': cart.dir = '<'
        of '+':
          if cart.turn == 0: cart.dir = '>'; cart.turn = 1
          elif cart.turn == 1: cart.turn = 2
          else: cart.dir = '<'; cart.turn = 0
        else: discard
      cart.y += 1
    else:
      echo "Error: Invalid cart direction"


proc solve(): void =
  let lines = readFile("input.txt").splitLines()
  var track: seq[seq[char]]
  var carts: seq[Cart]

  for line in lines:
    var row: seq[char]
    for c in line:
      case c:
        of '>': row.add('-'); carts.add(Cart(x: row.len - 1, y: track.len, dir: '>', turn: 0))
        of '<': row.add('-'); carts.add(Cart(x: row.len - 1, y: track.len, dir: '<', turn: 0))
        of '^': row.add('|'); carts.add(Cart(x: row.len - 1, y: track.len, dir: '^', turn: 0))
        of 'v': row.add('|'); carts.add(Cart(x: row.len - 1, y: track.len, dir: 'v', turn: 0))
        else: row.add(c)
    track.add(row)

  var collision = false
  while not collision:
    sort(carts, proc (a, b: Cart): int =
      if a.y < b.y: return -1
      elif a.y > b.y: return 1
      else: return cmp(a.x, b.x)
    )
    for i in 0..<carts.len:
      moveCart(track, carts[i])
    for i in 0..<carts.len:
      for j in i + 1..<carts.len:
        if carts[i].x == carts[j].x and carts[i].y == carts[j].y:
          echo carts[i].x, ",", carts[i].y
          collision = true
          break
      if collision: break

solve()
