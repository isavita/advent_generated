
fs = require 'fs'

# Helper function to calculate energy cost
energyCost = (type) ->
  switch type
    when 'A' then 1
    when 'B' then 10
    when 'C' then 100
    when 'D' then 1000
    else 0

# Helper function to represent the state as a string for memoization
stateToString = (hallway, rooms) ->
    hallway.join('') + rooms.map((room) -> room.join('')).join('')

# Helper function to check if a room is ready to receive amphipods
isRoomReady = (rooms, roomIndex, type) ->
  rooms[roomIndex].every (cell) -> cell == '.' || cell == type

# Main function to solve the puzzle using Dijkstra's algorithm
solve = (initialHallway, initialRooms) ->
  initialState = stateToString(initialHallway, initialRooms)
  distances = {}
  distances[initialState] = 0
  queue = [{ hallway: initialHallway, rooms: initialRooms, cost: 0 }]

  while queue.length > 0
    # Find the state with the minimum cost (Dijkstra)
    minIndex = 0
    for i in [1...queue.length]
      if queue[i].cost < queue[minIndex].cost
        minIndex = i
    { hallway, rooms, cost } = queue.splice(minIndex, 1)[0]
    currentState = stateToString(hallway, rooms)
    
    #If we reach a cost worse that the current cost in the distances, we continue
    continue if distances[currentState] < cost

    # Check if we've reached the goal state
    if rooms.every (room, i) -> room.every (cell) -> cell == ['A', 'B', 'C', 'D'][i]
      return cost

    # Move amphipods from rooms to hallway
    for roomIndex in [0...rooms.length]
      room = rooms[roomIndex]
      for depth in [0...room.length]
        if room[depth] != '.'
          type = room[depth]
          for hallPos in [0...hallway.length]
            # Check for valid hallway positions (not directly in front of a room)
            continue if [2, 4, 6, 8].includes hallPos

            # Check if path is clear
            pathClear = true
            start = Math.min(hallPos, 2 + roomIndex * 2)
            end = Math.max(hallPos, 2 + roomIndex * 2)

            for i in [start...end + 1]
              if i != 2 + roomIndex * 2 and hallway[i] != '.'
                pathClear = false
                break
            for i in [0...depth]
                if room[i] != '.' and i != depth
                    pathClear = false
                    break

            if pathClear
              newHallway = hallway.slice()
              newRooms = rooms.map (r) -> r.slice()
              newHallway[hallPos] = type
              newRooms[roomIndex][depth] = '.'
              newCost = cost + (Math.abs(hallPos - (2 + roomIndex * 2)) + depth + 1) * energyCost(type)
              newState = stateToString(newHallway, newRooms)

              if not distances[newState]? or newCost < distances[newState]
                distances[newState] = newCost
                queue.push { hallway: newHallway, rooms: newRooms, cost: newCost }
          break  # Only move the topmost amphipod

    # Move amphipods from hallway to rooms
    for hallPos in [0...hallway.length]
      if hallway[hallPos] != '.'
        type = hallway[hallPos]
        roomIndex = type.charCodeAt(0) - 'A'.charCodeAt(0)
        room = rooms[roomIndex]

        if isRoomReady(rooms, roomIndex, type)
          # Check if path is clear
          pathClear = true
          start = Math.min(hallPos, 2 + roomIndex * 2)
          end = Math.max(hallPos, 2 + roomIndex * 2)
          for i in [start...end + 1]
             if i != hallPos and hallway[i] != '.'
              pathClear = false
              break

          if pathClear
            # Find the deepest available slot in the room
            targetDepth = -1;
            for d in [0..room.length-1]
                if room[d] == '.'
                    targetDepth = d
                else
                    break
            if targetDepth != -1
              newHallway = hallway.slice()
              newRooms = rooms.map (r) -> r.slice()
              newHallway[hallPos] = '.'
              newRooms[roomIndex][targetDepth] = type
              newCost = cost + (Math.abs(hallPos - (2 + roomIndex * 2)) + targetDepth + 1) * energyCost(type)
              newState = stateToString(newHallway, newRooms)

              if not distances[newState]? or newCost < distances[newState]
                distances[newState] = newCost
                queue.push { hallway: newHallway, rooms: newRooms, cost: newCost }
  return -1 # Should not happen for well-defined puzzles


# Read input from file
inputFile = 'input.txt'
input = fs.readFileSync(inputFile, 'utf8').trim().split('\n')

# Parse input for part 1
initialHallway1 = ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
initialRooms1 = [
  [input[2][3], input[3][3]],
  [input[2][5], input[3][5]],
  [input[2][7], input[3][7]],
  [input[2][9], input[3][9]],
]

# Solve part 1
part1Result = solve(initialHallway1, initialRooms1)
console.log "Part 1: #{part1Result}"


# Parse input for Part 2, inserting additional lines
initialHallway2 = ['.','.','.','.','.','.','.','.','.','.','.']
initialRooms2 = [
  [input[2][3], 'D', 'D', input[3][3]],
  [input[2][5], 'C', 'B', input[3][5]],
  [input[2][7], 'B', 'A', input[3][7]],
  [input[2][9], 'A', 'C', input[3][9]]
]


# Solve Part 2
part2Result = solve(initialHallway2, initialRooms2)
console.log "Part 2: #{part2Result}"
