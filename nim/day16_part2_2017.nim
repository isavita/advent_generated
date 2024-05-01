import strutils, sequtils

type
  Move = enum
    Spin, Exchange, Partner

  DanceMove = tuple
    move: Move
    arg1: int
    arg2: int
    prog1: char
    prog2: char

proc parseMove(line: string): DanceMove =
  if line[0] == 's':
    result = (move: Spin, arg1: parseInt(line[1..^1]), arg2: 0, prog1: '\0', prog2: '\0')
  elif line[0] == 'x':
    let parts = line[1..^1].split('/')
    result = (move: Exchange, arg1: parseInt(parts[0]), arg2: parseInt(parts[1]), prog1: '\0', prog2: '\0')
  else:
    let parts = line[1..^1].split('/')
    result = (move: Partner, arg1: 0, arg2: 0, prog1: parts[0][0], prog2: parts[1][0])

proc dance(moves: seq[DanceMove], programs: var seq[char]) =
  for move in moves:
    case move.move
    of Spin:
      let n = move.arg1
      programs = programs[programs.len - n .. ^1] & programs[0 .. programs.len - n - 1]
    of Exchange:
      let i = move.arg1
      let j = move.arg2
      swap(programs[i], programs[j])
    of Partner:
      let i = programs.find(move.prog1)
      let j = programs.find(move.prog2)
      swap(programs[i], programs[j])

proc main =
  let input = readFile("input.txt").strip().split(',')
  var programs = toSeq('a'..'p')
  var moves: seq[DanceMove]

  for line in input:
    moves.add(parseMove(line))

  for _ in 1..1000000000 mod 60:
    dance(moves, programs)

  echo programs.join("")

when isMainModule:
  main()