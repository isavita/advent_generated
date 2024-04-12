fs = require 'fs'

spin = (programs, x) ->
  n = programs.length
  temp = programs.slice()
  for i in [0...n]
    programs[(i + x) % n] = temp[i]

exchange = (programs, A, B) ->
  [programs[A], programs[B]] = [programs[B], programs[A]]

partner = (programs, A, B) ->
  indexA = programs.indexOf A
  indexB = programs.indexOf B
  exchange programs, indexA, indexB

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  moves = data.trim().split ","
  programs = Array.from "abcdefghijklmnop"
  initial = programs.join ""
  cycleLen = 0

  for i in [0...1000000000]
    for move in moves
      switch move[0]
        when 's'
          x = parseInt move[1..]
          spin programs, x
        when 'x'
          positions = move[1..].split "/"
          A = parseInt positions[0]
          B = parseInt positions[1]
          exchange programs, A, B
        when 'p'
          A = move[1]
          B = move[3]
          partner programs, A, B

    if programs.join("") is initial
      cycleLen = i + 1
      break

  # Reset to initial state
  programs = Array.from initial

  # Perform the dance (10^9 mod cycleLen) times
  for i in [0...1000000000 % cycleLen]
    for move in moves
      switch move[0]
        when 's'
          x = parseInt move[1..]
          spin programs, x
        when 'x'
          positions = move[1..].split "/"
          A = parseInt positions[0]
          B = parseInt positions[1]
          exchange programs, A, B
        when 'p'
          A = move[1]
          B = move[3]
          partner programs, A, B

  console.log programs.join("")