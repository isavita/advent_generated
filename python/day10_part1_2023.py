
from typing import List, Tuple

Coord = Tuple[int, int]
Tile = str
Pipe = dict

Undefined = (0, 0)
Top = (0, -1)
Right = (1, 0)
Bottom = (0, 1)
Left = (-1, 0)

Empty = '.'
Start = 'S'
Vertical = '|'
Horizontal = '-'
TopLeftCorner = 'J'
TopRightCorner = 'L'
BottomLeftCorner = '7'
BottomRightCorner = 'F'
Enclosed = 'X'

VerticalPipe = {Top, Bottom}
HorizontalPipe = {Left, Right}
TopLeftCornerPipe = {Top, Left}
TopRightCornerPipe = {Top, Right}
BottomLeftCornerPipe = {Bottom, Left}
BottomRightCornerPipe = {Bottom, Right}

TileToPipe = {
    Vertical: VerticalPipe,
    Horizontal: HorizontalPipe,
    TopLeftCorner: TopLeftCornerPipe,
    TopRightCorner: TopRightCornerPipe,
    BottomLeftCorner: BottomLeftCornerPipe,
    BottomRightCorner: BottomRightCornerPipe
}

def getPipeFromTile(tile: Tile) -> Pipe:
    return TileToPipe.get(tile, {})

def getTileFromPipe(pipe: Pipe) -> Tile:
    for tile, associatedPipe in TileToPipe.items():
        if pipe == associatedPipe:
            return tile
    return Empty

def buildGrid(input: List[str]) -> dict:
    grid = {}
    for y, line in enumerate(input):
        for x, char in enumerate(line):
            if char != Empty:
                grid[(x, y)] = char
    return grid

def findStart(grid: dict) -> Coord:
    for coord, value in grid.items():
        if value == Start:
            return coord
    return Undefined

def getPipeFromNeighbors(coord: Coord, grid: dict) -> Pipe:
    pipe = {}
    possibleNeighbors = {
        Top: (coord[0] + Top[0], coord[1] + Top[1]),
        Right: (coord[0] + Right[0], coord[1] + Right[1]),
        Bottom: (coord[0] + Bottom[0], coord[1] + Bottom[1]),
        Left: (coord[0] + Left[0], coord[1] + Left[1])
    }
    for dir, neighborCoord in possibleNeighbors.items():
        if neighborCoord in grid:
            neighborPipe = getPipeFromTile(grid[neighborCoord])
            if (dir[0] * -1, dir[1] * -1) in neighborPipe:
                pipe[dir] = True
    return pipe

def pathFinding(start: Coord, grid: dict) -> List[Coord]:
    path = [start]
    startPipe = getPipeFromNeighbors(start, grid)

    previousDir = None
    current = None
    for dir in startPipe:
        previousDir = dir
        current = (start[0] + dir[0], start[1] + dir[1])

    while current != start:
        path.append(current)
        currentPipe = getPipeFromTile(grid[current])
        for dir in currentPipe:
            if dir != (previousDir[0] * -1, previousDir[1] * -1):
                previousDir = dir
                current = (current[0] + dir[0], current[1] + dir[1])
                break
    return path

def getPathGrid(grid: dict, path: List[Coord], empty: Tile) -> dict:
    newGrid = grid.copy()
    for coord in path:
        newGrid[coord] = grid[coord]

    start = path[0]
    newGrid[start] = getTileFromPipe(getPipeFromNeighbors(start, grid))

    return newGrid

def isInside(coord: Coord, grid: dict, empty: Tile) -> bool:
    if coord in grid:
        return False

    startPipe = empty
    numPipeOnLeft = 0
    for x in range(coord[0]):
        coord = (x, coord[1])
        v = grid.get(coord, None)
        if v == Vertical:
            numPipeOnLeft += 1
        elif v == TopRightCorner:
            startPipe = TopRightCorner
        elif v == BottomRightCorner:
            startPipe = BottomRightCorner
        elif v == TopLeftCorner:
            if startPipe == BottomRightCorner:
                startPipe = empty
                numPipeOnLeft += 1
            elif v == TopRightCorner:
                startPipe = Empty
        elif v == BottomLeftCorner:
            if startPipe == TopRightCorner:
                startPipe = Empty
                numPipeOnLeft += 1
            elif startPipe == BottomRightCorner:
                startPipe = Empty

    return numPipeOnLeft % 2 == 1

def solve(input: List[str]) -> int:
    grid = buildGrid(input)
    start = findStart(grid)
    path = pathFinding(start, grid)
    numPipesVisited = len(path)
    maxLength = numPipesVisited // 2
    return maxLength

def readFile(fileName: str) -> List[str]:
    with open(fileName, 'r') as file:
        return [line.strip() for line in file.readlines()]

def main():
    input = readFile("input.txt")
    print(solve(input))

if __name__ == "__main__":
    main()
