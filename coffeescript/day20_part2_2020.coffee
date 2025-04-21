
# Import the Node.js file system module
fs = require 'fs'

# --- Helper Functions ---

# Reverse a string
reverseString = (s) -> s.split('').reverse().join('')

# Rotate a 2D grid (array of strings) 90 degrees clockwise
rotateGrid = (grid) ->
  size = grid.length
  newGrid = for r in [0...size]
    (for c in [size-1..0] by -1 then grid[c][r]).join('')
  newGrid

# Flip a 2D grid horizontally
flipGrid = (grid) ->
  (row.split('').reverse().join('') for row in grid)

# --- Tile Class ---

class Tile
  constructor: (@id, @grid) ->
    @size = @grid.length

  # Get a specific border string
  getBorder: (side) ->
    switch side
      when 'top'    then @grid[0]
      when 'bottom' then @grid[@size - 1]
      when 'left'   then (row[0] for row in @grid).join('')
      when 'right'  then (row[@size - 1] for row in @grid).join('')

  # Get all four borders [top, right, bottom, left]
  getBorders: -> [@getBorder('top'), @getBorder('right'), @getBorder('bottom'), @getBorder('left')]

  # Rotate the tile (returns a new Tile instance)
  rotate: ->
    new Tile(@id, rotateGrid(@grid))

  # Flip the tile horizontally (returns a new Tile instance)
  flip: ->
    new Tile(@id, flipGrid(@grid))

  # Generate all 8 possible orientations of the tile
  getOrientations: ->
    orientations = []
    current = this
    for _ in [0...4] # 4 rotations
      orientations.push(current)
      orientations.push(current.flip()) # Flipped version of each rotation
      current = current.rotate()
    # Deduplicate based on grid content if necessary, though usually distinct
    # For this problem, keeping all 8 is fine as matching logic handles it.
    return orientations

  # Get the inner grid without borders
  getInnerGrid: ->
    (row[1...@size-1] for row in @grid[1...@size-1])

# --- Main Logic ---

# Parse input data into a map of {id: Tile}
parseInput = (data) ->
  tilesData = data.trim().split('\n\n')
  tilesMap = {}
  for tileBlock in tilesData
    lines = tileBlock.split('\n')
    id = parseInt(lines[0].match(/(\d+)/)[1])
    grid = lines[1..]
    tilesMap[id] = new Tile(id, grid)
  tilesMap

# --- Part 1: Assembly ---

# Backtracking function to assemble the image
solvePuzzle = (tiles) ->
  tileList = Object.values(tiles)
  gridSize = Math.sqrt(tileList.length) | 0
  unless gridSize * gridSize == tileList.length
    throw new Error("Input does not form a square grid")

  solutionGrid = Array(gridSize) # Will hold the correctly oriented Tile objects
  usedTileIds = new Set()

  # Recursive solver function
  solve = (r, c) ->
    # Base case: Puzzle solved
    return solutionGrid if r == gridSize

    # Calculate next position
    next_r = r
    next_c = c + 1
    if next_c == gridSize
      next_r = r + 1
      next_c = 0

    # Iterate through all available tiles
    for tile in tileList
      continue if usedTileIds.has(tile.id)

      # Try all 8 orientations for the current tile
      for orientedTile in tile.getOrientations()
        # Check compatibility with tile above
        if r > 0
          tileAbove = solutionGrid[r-1][c]
          unless orientedTile.getBorder('top') == tileAbove.getBorder('bottom')
            continue # Borders don't match

        # Check compatibility with tile to the left
        if c > 0
          tileLeft = solutionGrid[r][c-1]
          unless orientedTile.getBorder('left') == tileLeft.getBorder('right')
            continue # Borders don't match

        # If compatible, place the tile and recurse
        solutionGrid[r] ?= Array(gridSize) # Ensure row array exists
        solutionGrid[r][c] = orientedTile
        usedTileIds.add(tile.id)

        # Recurse to the next position
        result = solve(next_r, next_c)
        return result if result # Solution found

        # Backtrack: Unplace the tile and try the next one/orientation
        usedTileIds.delete(tile.id)
        solutionGrid[r][c] = null # Optional cleanup

    # No solution found from this state
    return null

  # Start the backtracking from the top-left corner (0, 0)
  return solve(0, 0)

# --- Part 2: Sea Monsters ---

# Sea monster pattern definition
# Relative coordinates (dr, dc) from the top-left of the pattern
monsterPattern = [
  [0, 18],
  [1, 0], [1, 5], [1, 6], [1, 11], [1, 12], [1, 17], [1, 18], [1, 19],
  [2, 1], [2, 4], [2, 7], [2, 10], [2, 13], [2, 16]
]
monsterWidth = 20 # Max width of the pattern
monsterHeight = 3  # Max height of the pattern
monsterHashCount = monsterPattern.length

# Create the final large image by stitching inner tiles
createFinalImage = (solutionGrid) ->
  finalImage = []
  tileSize = solutionGrid[0][0].size # Assuming all tiles are same size
  innerSize = tileSize - 2
  gridSize = solutionGrid.length

  for tileRow in [0...gridSize]
    stitchedRows = for _ in [0...innerSize] then '' # Initialize innerSize empty rows
    for tileCol in [0...gridSize]
      innerGrid = solutionGrid[tileRow][tileCol].getInnerGrid()
      for r in [0...innerSize]
        stitchedRows[r] += innerGrid[r]
    finalImage.push(...stitchedRows) # Append the stitched rows for this tile row
  finalImage

# Find and mark sea monsters in a given image grid orientation
findAndMarkMonsters = (imageGrid) ->
  rows = imageGrid.length
  cols = imageGrid[0].length
  isMonster = Array(rows) # 2D array to mark monster locations
  for r in [0...rows] then isMonster[r] = Array(cols).fill(false)
  monsterFound = false

  for r in [0...rows - monsterHeight + 1]
    for c in [0...cols - monsterWidth + 1]
      isMatch = true
      for [dr, dc] in monsterPattern
        unless imageGrid[r + dr][c + dc] == '#'
          isMatch = false
          break
      
      if isMatch
        monsterFound = true
        # Mark the coordinates belonging to this monster
        for [dr, dc] in monsterPattern
          isMonster[r + dr][c + dc] = true

  return [monsterFound, isMonster]

# Calculate water roughness
calculateRoughness = (imageGrid, isMonster) ->
  rows = imageGrid.length
  cols = imageGrid[0].length
  roughness = 0
  for r in [0...rows]
    for c in [0...cols]
      if imageGrid[r][c] == '#' and not isMonster[r][c]
        roughness += 1
  roughness

# --- Main Execution ---

main = ->
  try
    # Read input file
    data = fs.readFileSync('input.txt', 'utf8')

    # Parse tiles
    tilesMap = parseInput(data)

    # --- Part 1 ---
    console.log "Assembling puzzle..."
    solutionGrid = solvePuzzle(tilesMap)

    unless solutionGrid
      console.error "Failed to solve the puzzle!"
      return

    console.log "Puzzle assembled!"
    gridSize = solutionGrid.length
    # Get corner IDs
    topLeftId = solutionGrid[0][0].id
    topRightId = solutionGrid[0][gridSize - 1].id
    bottomLeftId = solutionGrid[gridSize - 1][0].id
    bottomRightId = solutionGrid[gridSize - 1][gridSize - 1].id

    cornerProduct = topLeftId * topRightId * bottomLeftId * bottomRightId
    console.log "Part 1: Product of corner tile IDs: #{cornerProduct}"

    # --- Part 2 ---
    console.log "Creating final image..."
    finalImage = createFinalImage(solutionGrid)

    console.log "Searching for sea monsters..."
    roughness = -1
    currentImage = finalImage

    # Try all 8 orientations of the final image
    for flipPass in [0...2]
      for rotatePass in [0...4]
        # Search in current orientation
        [found, isMonster] = findAndMarkMonsters(currentImage)

        if found
          console.log "Sea monsters found in an orientation!"
          roughness = calculateRoughness(currentImage, isMonster)
          break # Exit inner loop

        # Rotate for next iteration
        currentImage = rotateGrid(currentImage)
      
      break if roughness != -1 # Exit outer loop if found

      # Flip for the second pass
      currentImage = flipGrid(currentImage) # Flip the (rotated back to original) image


    if roughness != -1
      console.log "Part 2: Water roughness: #{roughness}"
    else
      console.log "Part 2: No sea monsters found in any orientation."

  catch err
    console.error "An error occurred:", err
    process.exit(1) # Indicate failure

# Run the main function
main()
