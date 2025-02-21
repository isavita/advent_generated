
fs = require 'fs'

# Function to check if an update is in the correct order
is_correct_order = (rules, update) ->
  for i in [0...update.length]
    for j in [i + 1...update.length]
      if rules[update[j]]?.has(update[i])
        return false
  true

# Function to perform topological sort
topological_sort = (rules, update) ->
  in_degree = {}
  for page in update
    in_degree[page] = 0

  for page1 in update
    for page2 in update
      if rules[page2]?.has(page1)
        in_degree[page1] += 1

  queue = []
  for page in update
    if in_degree[page] == 0
      queue.push page

  sorted_update = []
  while queue.length > 0
    page = queue.shift()
    sorted_update.push page

    for next_page in update
      if rules[page]?.has(next_page)
        in_degree[next_page] -= 1
        if in_degree[next_page] == 0
          queue.push next_page

  if sorted_update.length != update.length #cycle detection
    return update

  sorted_update

# Read input from file
input = fs.readFileSync('input.txt', 'utf8').trim().split('\n\n')
rule_lines = input[0].split('\n')
update_lines = input[1].split('\n')

# Parse rules
rules = {}
for line in rule_lines
  [before, after] = line.split('|').map (str) -> parseInt(str)
  rules[before] = rules[before] or new Set()
  rules[before].add after

# Process updates
correct_middle_sum = 0
incorrect_middle_sum = 0

for line in update_lines
  update = line.split(',').map (str) -> parseInt(str)
    
  if is_correct_order(rules, update)
    middle_index = Math.floor(update.length / 2)
    correct_middle_sum += update[middle_index]
  else
    sorted_update = topological_sort(rules, update)
    middle_index = Math.floor(sorted_update.length / 2)
    incorrect_middle_sum += sorted_update[middle_index]

# Print results
console.log "Part 1: #{correct_middle_sum}"
console.log "Part 2: #{incorrect_middle_sum}"
