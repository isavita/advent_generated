fs = require 'fs'

class Cart
  constructor: (@x, @y, @direction, @turns) ->

class Position
  constructor: (@x, @y) ->

moveCart = (cart, tracks) ->
  switch cart.direction
    when '>' then cart.x++
    when '<' then cart.x--
    when '^' then cart.y--
    when 'v' then cart.y++

  switch tracks[cart.y][cart.x]
    when '+' then turnCart(cart)
    when '/', '\\' then changeDirection(cart, tracks[cart.y][cart.x])

turnCart = (cart) ->
  if cart.turns % 3 == 0
    cart.direction = switch cart.direction
      when '>' then '^'
      when '<' then 'v'
      when '^' then '<'
      else '>'
  else if cart.turns % 3 == 2
    cart.direction = switch cart.direction
      when '>' then 'v'
      when '<' then '^'
      when '^' then '>'
      else '<'
  cart.turns++

changeDirection = (cart, track) ->
  if track == '/'
    cart.direction = switch cart.direction
      when '>' then '^'
      when '<' then 'v'
      when '^' then '>'
      else '<'
  else if track == '\\'
    cart.direction = switch cart.direction
      when '>' then 'v'
      when '<' then '^'
      when '^' then '<'
      else '>'

checkCrash = (cart, carts) ->
  for c, i in carts
    if c != cart && c.x == cart.x && c.y == cart.y
      return i
  -1

fileContent = fs.readFileSync('input.txt', 'utf8').split('\n')
tracks = []
carts = []

for line, y in fileContent
  trackLine = line.split('')
  for char, x in trackLine
    switch char
      when '>', '<', '^', 'v'
        carts.push new Cart(x, y, char, 0)
        if char in ['>', '<']
          trackLine[x] = '-'
        else
          trackLine[x] = '|'
  tracks.push trackLine

while carts.length > 1
  carts.sort (a, b) ->
    if a.y == b.y
      a.x - b.x
    else
      a.y - b.y

  toRemove = {}
  for cart, i in carts
    if i of toRemove
      continue

    moveCart(cart, tracks)
    crashIndex = checkCrash(cart, carts)
    if crashIndex != -1
      toRemove[i] = true
      toRemove[crashIndex] = true

  carts = (cart for cart, i in carts when not (i of toRemove))

console.log "#{carts[0].x},#{carts[0].y}"