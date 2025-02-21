
fs = require 'fs'
class Halves
  constructor: (@is_chip, @material) ->
class State
  constructor: (@floors = ([[], [], [], []]), @elevator_level = 0, @steps = 0) ->
  hash_key: ->
    gen_to_index = {}
    chip_to_index = {}
    for fl_index, floor of @floors
      for half in floor
        if half.is_chip
          chip_to_index[half.material] = fl_index
        else
          gen_to_index[half.material] = fl_index
    gen_chip_pairs = (([gen_to_index[mat], chip_to_index[mat]] for mat of gen_to_index).sort (a, b) ->
      return -1 if a[0] < b[0] or (a[0] is b[0] and a[1] < b[1])
      return 1)
    "#{ @elevator_level }#{ gen_chip_pairs }"
  is_valid: ->
    for floor in @floors
      gens_seen = (half.material for half in floor when not half.is_chip)
      continue if gens_seen.length is 0
      for half in floor
        return false if half.is_chip and half.material not in gens_seen
    true
  is_done: ->
    (floor.length is 0 for floor in @floors[...-1]).every (x) -> x
  get_movable_perm_indices: ->
    current_level = @floors[@elevator_level]
    indices = [0...current_level.length]
    result = []
    for r in [1, 2]
      c = (p for p in @combinations indices, r)
      result.push x for x in c
    result
  clone: ->
    new State((floor.slice() for floor in @floors), @elevator_level, @steps)
  get_next_states: ->
    future_states = []
    movable_perm_indices = @get_movable_perm_indices()
    ele_diffs = (d for d in [-1, 1] when 0 <= @elevator_level + d < @floors.length)
    for ele_diff in ele_diffs
      for perm_indices in movable_perm_indices
        cl = @clone()
        cl.elevator_level += ele_diff
        cl.steps += 1
        old_level = @elevator_level
        new_level = cl.elevator_level
        for index in perm_indices
          cl.floors[new_level].push cl.floors[old_level][index]
        for index in perm_indices.sort((a, b) -> b - a)
          cl.floors[old_level].splice index, 1
        future_states.push cl if cl.is_valid()
    future_states
  combinations: (arr, n) ->
    return [[]] if n is 0
    result = []
    for i in [0...arr.length]
      first = arr[i]
      rest = arr.slice(i + 1)
      for combination in @combinations rest, n - 1
        result.push [first].concat combination
    result
rtg_hell_day = (input_str, part) ->
  lines = input_str.trim().split '\n'
  floors = [[], [], [], []]
  for line_index, line of lines
    parts = line.replace(/,/g, '').replace(/\./g, '').split ' '
    for i, word of parts
      if word is "generator"
        material = parts[i - 1]
        floors[line_index].push new Halves(false, material)
      else if word is "microchip"
        material = parts[i - 1].split("-comp")[0]
        floors[line_index].push new Halves(true, material)
  initial_state = new State(floors)
  if part is 2
    initial_state.floors[0] = initial_state.floors[0].concat [
      new Halves(false, "elerium")
      new Halves(true, "elerium")
      new Halves(false, "dilithium")
      new Halves(true, "dilithium")
    ]
  queue = [initial_state]
  prev_states = new Set()
  while queue.length > 0
    front = queue.shift()
    return front.steps if front.is_done()
    hash_key = front.hash_key()
    continue if prev_states.has hash_key
    prev_states.add hash_key
    queue = queue.concat front.get_next_states()
  -1
input_str = fs.readFileSync('input.txt', 'utf8')
console.log rtg_hell_day(input_str, 2)
