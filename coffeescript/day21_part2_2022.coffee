fs = require 'fs'

class Monkey
  constructor: (@name, @val, @hasVal, @left, @right, @op) ->

  solve: ->
    if @hasVal
      return [@val, true]

    if @left? and @right?
      [left, lOk] = @left.solve()
      [right, rOk] = @right.solve()

      if lOk and rOk
        switch @op
          when '+' then return [left + right, true]
          when '-' then return [left - right, true]
          when '*' then return [left * right, true]
          when '/' then return [left / right, true]
          when '=='
            if left == right
              return [0, true]
            else
              return [1, true]
    return [0, false]

  expect: (x) ->
    if @name == 'humn'
      return x

    [left, lOk] = @left.solve()
    [right, rOk] = @right.solve()

    if not lOk
      switch @op
        when '+' then return @left.expect(x - right)
        when '-' then return @left.expect(x + right)
        when '*' then return @left.expect(x / right)
        when '/' then return @left.expect(x * right)
        when '==' then return @left.expect(right)

    if not rOk
      switch @op
        when '+' then return @right.expect(x - left)
        when '-' then return @right.expect(left - x)
        when '*' then return @right.expect(x / left)
        when '/' then return @right.expect(left / x)
        when '==' then return @right.expect(left)

    throw new Error('impossible')

parse = ->
  index = {}

  initMonkey = (s) ->
    index[s] ?= new Monkey(s)

  data = fs.readFileSync('input.txt', 'utf8').split('\n')

  for line in data
    [goal, expr] = line.split(': ')
    initMonkey(goal)

    num = parseInt(expr)
    if not isNaN(num)
      index[goal].val = num
      index[goal].hasVal = true
      continue

    [left, op, right] = expr.split(' ')
    initMonkey(left)
    initMonkey(right)

    index[goal].left = index[left]
    index[goal].op = op
    index[goal].right = index[right]

  return index

index = parse()
index['humn'].hasVal = false
index['root'].op = '=='

console.log(index['root'].expect(0))