fs = require 'fs'

spin = (programs, x) ->
  n = programs.length
  temp = programs.slice()
  for i in [0...n]
    programs[(i + x) % n] = temp[i]
  return

exchange = (programs, A, B) ->
  [programs[A], programs[B]] = [programs[B], programs[A]]
  return

partner = (programs, A, B) ->
  indexA = programs.indexOf A
  indexB = programs.indexOf B
  exchange programs, indexA, indexB
  return

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  moves = data.trim().split ","
  programs = "abcdefghijklmnop".split ""

  for move in moves
    switch move[0]
      when 's'
        x = parseInt move[1...]
        spin programs, x
      when 'x'
        positions = move[1...].split "/"
        A = parseInt positions[0]
        B = parseInt positions[1]
        exchange programs, A, B
      when 'p'
        positions = move[1...].split "/"
        A = positions[0]
        B = positions[1]
        partner programs, A, B

  console.log programs.join ""