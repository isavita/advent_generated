
import std/[md5, strutils, tables, os, options]

# --- Constants ---
const KeyCount = 64
const Lookahead = 1000
const StretchCount = 2016

# --- Helper Procedures ---

proc findTriple(s: string): Option[char] =
  ## Finds the first character that repeats 3 times consecutively in a string.
  if s.len < 3:
    return none(char)
  for i in 0 ..< s.len - 2:
    if s[i] == s[i+1] and s[i] == s[i+2]:
      return some(s[i])
  return none(char)

proc containsQuintuple(s: string, c: char): bool =
  ## Checks if a string contains the character 'c' repeated 5 times consecutively.
  if s.len < 5:
    return false
  let quint = c.repeat(5)
  return s.contains(quint)

# --- Hashing Procedures ---

type HashCache = Table[int, string]

proc getHash(salt: string, index: int, cache: var HashCache): string =
  ## Calculates MD5 hash of salt & index, using cache.
  if index notin cache:
    cache[index] = md5.getMD5(salt & $index)
  return cache[index]

proc getStretchedHash(salt: string, index: int, cache: var HashCache): string =
  ## Calculates the stretched MD5 hash, using cache.
  if index notin cache:
    var hash = md5.getMD5(salt & $index)
    for _ in 1 .. StretchCount:
      hash = md5.getMD5(hash)
    cache[index] = hash
  return cache[index]

# --- Solving Procedures ---

type HashFunc = proc(salt: string, index: int, cache: var HashCache): string {.nimcall.}

proc findKeyIndex(salt: string, hashFunc: HashFunc): int =
  ## Finds the index that produces the 64th key using the specified hash function.
  var
    keysFound = 0
    index = 0
    cache: HashCache = initTable[int, string]()

  while keysFound < KeyCount:
    let currentHash = hashFunc(salt, index, cache)
    let tripleCharOpt = findTriple(currentHash)

    if tripleCharOpt.isSome:
      let tripleChar = tripleCharOpt.get()
      # Check the next 1000 hashes for a quintuple
      for j in index + 1 .. index + Lookahead:
        let nextHash = hashFunc(salt, j, cache)
        if containsQuintuple(nextHash, tripleChar):
          keysFound += 1
          # echo "Found key #", keysFound, " at index ", index, " (triple ", tripleChar, ") with quintuple at index ", j
          if keysFound == KeyCount:
            return index # Found the 64th key
          break # Stop checking for this tripleChar, move to next index

    index += 1
    # Optional: Clean up cache to prevent excessive memory use for very large inputs,
    # though likely unnecessary for this problem constraints.
    # if index > Lookahead * 2 and (index - Lookahead - 1) in cache:
    #   cache.del(index - Lookahead - 1)

  return -1 # Should not be reached if KeyCount > 0

# --- Main Entry Point ---

proc main(filename: string) =
  let salt = readFile(filename).strip()
  if salt.len == 0:
    echo "Error: Input file is empty or not found."
    quit(1)

  echo "Salt: ", salt

  # Part 1
  let part1Index = findKeyIndex(salt, getHash)
  echo "Part 1: The index producing the 64th key is ", part1Index

  # Part 2
  let part2Index = findKeyIndex(salt, getStretchedHash)
  echo "Part 2: The index producing the 64th stretched key is ", part2Index

when isMainModule:
  let filename = if paramCount() > 0: paramStr(1) else: "input.txt"
  main(filename)
