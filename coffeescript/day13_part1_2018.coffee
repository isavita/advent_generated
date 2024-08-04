fs = require 'fs'

class Cart
  constructor: (@x, @y, @dir) ->
    @turn = 0

moveRight = (track, cart) ->
  switch track[cart.y][cart.x + 1]
    when '\\' then cart.dir = 'v'
    when '/' then cart.dir = '^'
    when '+'
      if cart.turn == 0
        cart.dir = '^'
        cart.turn = 1
      else if cart.turn == 1
        cart.turn = 2
      else if cart.turn == 2
        cart.dir = 'v'
        cart.turn = 0
    when '-' then null
    else console.log "Error on track cart can't move:", cart.x + 1, cart.y, track[cart.y][cart.x + 1]
  cart.x++
  cart

moveLeft = (track, cart) ->
  switch track[cart.y][cart.x - 1]
    when '/' then cart.dir = 'v'
    when '\\' then cart.dir = '^'
    when '+'
      if cart.turn == 0
        cart.dir = 'v'
        cart.turn = 1
      else if cart.turn == 1
        cart.turn = 2
      else if cart.turn == 2
        cart.dir = '^'
        cart.turn = 0
    when '-' then null
    else console.log "Error on track cart can't move:", cart.x - 1, cart.y, track[cart.y][cart.x - 1]
  cart.x--
  cart

moveUp = (track, cart) ->
  switch track[cart.y - 1][cart.x]
    when '/' then cart.dir = '>'
    when '\\' then cart.dir = '<'
    when '+'
      if cart.turn == 0
        cart.dir = '<'
        cart.turn = 1
      else if cart.turn == 1
        cart.turn = 2
      else if cart.turn == 2
        cart.dir = '>'
        cart.turn = 0
    when '|' then null
    else console.log "Error on track cart can't move:", cart.x, cart.y - 1, track[cart.y - 1][cart.x]
  cart.y--
  cart

moveDown = (track, cart) ->
  switch track[cart.y + 1][cart.x]
    when '/' then cart.dir = '<'
    when '\\' then cart.dir = '>'
    when '+'
      if cart.turn == 0
        cart.dir = '>'
        cart.turn = 1
      else if cart.turn == 1
        cart.turn = 2
      else if cart.turn == 2
        cart.dir = '<'
        cart.turn = 0
    when '|' then null
    else console.log "Error on track cart can't move:", cart.x, cart.y + 1, track[cart.y + 1][cart.x]
  cart.y++
  cart

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  lines = data.split '\n'
  track = []
  carts = []

  for line, i in lines
    track.push []
    for s, j in line
      switch s
        when '>'
          track[i].push '-'
          carts.push new Cart j, i, '>'
        when '<'
          track[i].push '-'
          carts.push new Cart j, i, '<'
        when '^'
          track[i].push '|'
          carts.push new Cart j, i, '^'
        when 'v'
          track[i].push '|'
          carts.push new Cart j, i, 'v'
        else track[i].push s

  collision = false
  while not collision
    for cart, i in carts
      switch cart.dir
        when '>' then carts[i] = moveRight track, cart
        when '<' then carts[i] = moveLeft track, cart
        when '^' then carts[i] = moveUp track, cart
        when 'v' then carts[i] = moveDown track, cart

    for cart, i in carts
      for otherCart in carts[i+1..]
        if cart.x == otherCart.x and cart.y == otherCart.y
          collision = true
          console.log "#{cart.x},#{cart.y}"