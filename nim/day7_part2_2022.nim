
import std/[strutils, sequtils, parseutils, math, os]

# --- Type Definitions ---

type
  NodeKind = enum
    nkDir, nkFile
  Node = ref object
    name: string
    kind: NodeKind
    size: int # For files: direct size; For dirs: calculated total size
    parent: Node # nil for root
    children: seq[Node]

# --- Helper Procedures ---

proc newNode(name: string, kind: NodeKind, size: int = 0, parent: Node = nil): Node =
  ## Creates a new Node object.
  new(result)
  result.name = name
  result.kind = kind
  result.size = size
  result.parent = parent
  result.children = @[]

proc findChild(parent: Node, name: string): Node =
  ## Finds a child node by name within a parent node.
  ## Assumes child exists based on valid input.
  for child in parent.children:
    if child.name == name:
      return child
  # This should ideally not happen with valid AoC input
  raise newException(ValueError, "Child '" & name & "' not found in '" & parent.name & "'")

proc calculateSize(node: Node): int =
  ## Recursively calculates the total size of a node (file or directory).
  ## Stores the calculated size in the node's size field for directories.
  if node.kind == nkFile:
    return node.size
  else: # nkDir
    var totalSize = 0
    for child in node.children:
      totalSize += calculateSize(child)
    node.size = totalSize # Store the calculated size for the directory
    return totalSize

proc sumSmallDirs(node: Node, threshold: int, currentSum: var int) =
    ## Recursively traverses the tree and adds sizes of directories
    ## at or below the threshold to currentSum.
    if node.kind == nkDir:
        if node.size <= threshold:
            currentSum += node.size
        for child in node.children:
            sumSmallDirs(child, threshold, currentSum)

proc findSmallestDirToDelete(node: Node, spaceNeeded: int, minSizeFound: var int) =
    ## Recursively traverses the tree to find the smallest directory
    ## whose size is at least spaceNeeded. Updates minSizeFound.
    if node.kind == nkDir:
        if node.size >= spaceNeeded:
            minSizeFound = min(minSizeFound, node.size)
        for child in node.children:
            findSmallestDirToDelete(child, spaceNeeded, minSizeFound)


# --- Main Program Logic ---

proc solve(lines: seq[string]): (int, int) =
  ## Parses the input, builds the filesystem tree, and calculates results.

  let root = newNode("/", nkDir)
  var cwd = root

  # Build the filesystem tree
  var i = 0
  while i < lines.len:
    let line = lines[i]
    if line.startsWith("$ cd"):
      let target = line.split(" ")[2]
      case target
      of "/":
        cwd = root
      of "..":
        if cwd.parent != nil: # Cannot go above root
          cwd = cwd.parent
      else:
        # Find or create the target directory if needed (though `ls` should define it first)
        # For simplicity, let's assume `ls` always runs before `cd` into a new dir.
        block findExisting:
            for child in cwd.children:
                if child.name == target and child.kind == nkDir:
                    cwd = child
                    break findExisting
            # If not found (shouldn't happen with valid input after ls)
            # Alternative: create it on the fly? Let's rely on ls creating it.
            raise newException(ValueError, "cd target '" & target & "' not found before ls in '" & cwd.name & "'")

      i += 1 # Move to the next line after processing cd

    elif line.startsWith("$ ls"):
      i += 1 # Move to the first line of ls output
      # Process ls output until the next command or end of input
      while i < lines.len and not lines[i].startsWith("$"):
        let outputLine = lines[i]
        let parts = outputLine.splitWhitespace()
        let name = parts[1]

        # Avoid adding duplicate nodes if `ls` is run multiple times in the same dir
        var exists = false
        for child in cwd.children:
            if child.name == name:
                exists = true
                break

        if not exists:
            if parts[0] == "dir":
              let newDir = newNode(name, nkDir, parent = cwd)
              cwd.children.add(newDir)
            else: # File
              let size = parseInt(parts[0])
              let newFile = newNode(name, nkFile, size = size, parent = cwd)
              cwd.children.add(newFile)
        i += 1 # Move to the next line of ls output or the next command

    else:
      # Should not happen if input starts with a command ($ cd /)
      echo "Warning: Unexpected line format: ", line
      i += 1

  # --- Calculations ---

  # Calculate all directory sizes first
  discard calculateSize(root) # Run the calculation, populating node.size for dirs

  # Part 1: Sum of sizes of directories with total size <= 100000
  var part1Result = 0
  sumSmallDirs(root, 100_000, part1Result)

  # Part 2: Find the smallest directory to delete
  let totalDiskSpace = 70_000_000
  let requiredUnusedSpace = 30_000_000
  let currentUsedSpace = root.size
  let currentUnusedSpace = totalDiskSpace - currentUsedSpace
  let spaceNeededToFree = requiredUnusedSpace - currentUnusedSpace

  var part2Result = totalDiskSpace # Initialize with a value larger than any possible dir size

  if spaceNeededToFree <= 0:
    part2Result = 0 # No deletion needed
  else:
    findSmallestDirToDelete(root, spaceNeededToFree, part2Result)

  return (part1Result, part2Result)

# --- Entry Point ---

when isMainModule:
  let inputPath = "input.txt"
  if not fileExists(inputPath):
    echo "Error: input.txt not found."
  else:
    let lines = readFile(inputPath).strip.splitLines
    if lines.len == 0:
      echo "Error: input.txt is empty."
    else:
      let (part1, part2) = solve(lines)
      echo part1 # Output for Part 1
      echo part2 # Output for Part 2

