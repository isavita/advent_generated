
import std/[strutils, sequtils, deques, os]

# --- Type Definitions ---
type
  Coord = tuple[r, c: int] # Represents grid coordinates (row, column)
  Grid = seq[string]        # Represents the garden map

# --- Helper Procedures ---

proc isInBounds(r, c, rows, cols: int): bool =
  ## Checks if the given coordinates (r, c) are within the grid boundaries.
  result = r >= 0 and r < rows and c >= 0 and c < cols

# --- Core Logic ---

proc findRegion(startR, startC: int, grid: Grid, visited: var seq[seq[bool]]): (int, int) =
  ## Performs a Breadth-First Search (BFS) starting from (startR, startC)
  ## to find a connected region of the same plant type.
  ## Updates the `visited` grid for the found region.
  ## Returns a tuple: (area: int, perimeter: int) of the region.

  let rows = grid.len
  let cols = grid[0].len # Assume non-empty grid checked before calling

  # If already visited or somehow out of bounds (shouldn't happen with caller logic),
  # return zero area/perimeter.
  if not isInBounds(startR, startC, rows, cols) or visited[startR][startC]:
    return (0, 0)

  let plantType = grid[startR][startC] # The type of plant in this region
  var area = 0
  var perimeter = 0
  var q = initDeque[Coord]() # Queue for BFS

  # Start BFS
  q.addLast((startR, startC))
  visited[startR][startC] = true

  while q.len > 0:
    let curr = q.popFirst()
    area += 1 # Increment area for the current plot

    # Define relative coordinates for neighbors (Up, Down, Left, Right)
    const dr = [-1, 1, 0, 0]
    const dc = [0, 0, -1, 1]

    # Check all 4 neighbors
    for i in 0..3:
      let nr = curr.r + dr[i]
      let nc = curr.c + dc[i]

      if isInBounds(nr, nc, rows, cols):
        # Neighbor is within grid bounds
        if grid[nr][nc] == plantType:
          # Neighbor is the same type - potential part of the region
          if not visited[nr][nc]:
            # If not visited yet, mark and add to queue
            visited[nr][nc] = true
            q.addLast((nr, nc))
        else:
          # Neighbor is a different type - this side contributes to perimeter
          perimeter += 1
      else:
        # Neighbor is out of bounds - this side contributes to perimeter
        perimeter += 1

  # Ensure non-zero area before returning to avoid division by zero if needed elsewhere,
  # though here we just need area > 0 for price calculation.
  if area == 0:
      return (0, 0)
  else:
      return (area, perimeter)

# --- Main Execution ---

proc main() =
  ## Reads input, processes the grid, calculates total fence price, and prints the result.
  let filename = "input.txt"

  # Check if input file exists
  if not fileExists(filename):
    echo "Error: Input file '", filename, "' not found."
    quit(1) # Exit with an error code

  # Read the grid from the file
  let grid = readFile(filename).strip().splitLines()

  # Handle empty grid case
  if grid.len == 0 or grid[0].len == 0:
    echo 0
    return

  let rows = grid.len
  let cols = grid[0].len

  # Initialize visited grid with all false
  var visited = newSeqWith(rows, newSeq[bool](cols))

  var totalPrice: int64 = 0 # Use int64 to prevent potential overflow

  # Iterate through each cell of the grid
  for r in 0..<rows:
    for c in 0..<cols:
      # If the cell hasn't been visited yet, it's the start of a new region
      if not visited[r][c]:
        # Find the region (area, perimeter) starting from this cell
        let (area, perimeter) = findRegion(r, c, grid, visited)

        # If a valid region was found (area > 0), calculate its price
        if area > 0:
          let regionPrice = int64(area) * int64(perimeter)
          totalPrice += regionPrice

  # Print the final total price
  echo totalPrice

# --- Program Entry Point ---
when isMainModule:
  main()
