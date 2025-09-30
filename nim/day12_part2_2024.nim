
import std/[strutils, sequtils, sets, tables]

type
  Pos = tuple[x, y: int]
  Region = object
    plant: char
    plots: HashSet[Pos]

proc readInput(filename: string): seq[seq[char]] =
  for line in filename.lines:
    if line.len > 0:
      result.add line.toSeq()

proc neighbors(p: Pos): array[4, Pos] =
  [(p.x-1, p.y), (p.x+1, p.y), (p.x, p.y-1), (p.x, p.y+1)]

proc findRegions(grid: seq[seq[char]]): seq[Region] =
  let h = grid.len
  let w = grid[0].len
  var visited = initHashSet[Pos]()
  
  for y in 0..<h:
    for x in 0..<w:
      let p = (x, y)
      if p in visited: continue
      
      var region = Region(plant: grid[y][x])
      var stack = @[p]
      
      while stack.len > 0:
        let cur = stack.pop()
        if cur in visited: continue
        visited.incl cur
        region.plots.incl cur
        
        for n in neighbors(cur):
          let (nx, ny) = n
          if nx >= 0 and ny >= 0 and nx < w and ny < h:
            if grid[ny][nx] == region.plant and n notin visited:
              stack.add n
      
      result.add region

proc calcPerimeter(region: Region): int =
  for p in region.plots:
    for n in neighbors(p):
      if n notin region.plots:
        inc result

proc countSides(region: Region): int =
  # Count corners (each corner = 1 side change)
  # For each plot, check its 4 corners
  for (x, y) in region.plots:
    # NW corner
    let n = (x-1, y) in region.plots
    let w = (x, y-1) in region.plots
    let nw = (x-1, y-1) in region.plots
    if (not n and not w) or (n and w and not nw):
      inc result
    
    # NE corner
    let e = (x, y+1) in region.plots
    let ne = (x-1, y+1) in region.plots
    if (not n and not e) or (n and e and not ne):
      inc result
    
    # SW corner
    let s = (x+1, y) in region.plots
    let sw = (x+1, y-1) in region.plots
    if (not s and not w) or (s and w and not sw):
      inc result
    
    # SE corner
    let se = (x+1, y+1) in region.plots
    if (not s and not e) or (s and e and not se):
      inc result

proc main() =
  let grid = readInput("input.txt")
  let regions = findRegions(grid)
  
  var part1 = 0
  var part2 = 0
  
  for region in regions:
    let area = region.plots.len
    let perimeter = calcPerimeter(region)
    let sides = countSides(region)
    
    part1 += area * perimeter
    part2 += area * sides
  
  echo part1
  echo part2

when isMainModule:
  main()
