import strutils

type
  Program = ref object
    id: int
    connections: seq[Program]

proc addConnection(p: Program, other: Program) =
  p.connections.add(other)
  other.connections.add(p)

proc dfs(p: Program, visited: var seq[bool]) =
  visited[p.id] = true
  for conn in p.connections:
    if not visited[conn.id]:
      dfs(conn, visited)

proc countGroups(programs: seq[Program]): int =
  var visited = newSeq[bool](programs.len)
  var groupCount = 0
  for p in programs:
    if not visited[p.id]:
      dfs(p, visited)
      groupCount.inc
  groupCount

when isMainModule:
  let input = readFile("input.txt").splitLines()
  var programs = newSeq[Program]()
  for line in input:
    let parts = line.split(" <-> ")
    let id = parseInt(parts[0])
    let connections = parts[1].split(", ")
    var program = Program(id: id, connections: newSeq[Program]())
    programs.add(program)
    for conn in connections:
      let connId = parseInt(conn)
      var connProgram: Program
      for p in programs:
        if p.id == connId:
          connProgram = p
          break
      if connProgram == nil:
        connProgram = Program(id: connId, connections: newSeq[Program]())
        programs.add(connProgram)
      addConnection(program, connProgram)

  echo "Part 1: ", countGroups(programs)
  echo "Part 2: ", countGroups(programs)