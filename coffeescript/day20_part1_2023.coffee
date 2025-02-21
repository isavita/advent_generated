
fs = require 'fs'

# Parse the input file
parseInput = (filename) ->
  modules = {}
  fs.readFileSync(filename, 'utf8').trim().split('\n').forEach (line) ->
    [module_name, destinations] = line.split(' -> ')
    destinations = destinations.split(', ')

    type = null
    name = module_name
    if module_name[0] == '%'
      type = 'flipflop'
      name = module_name[1..]
    else if module_name[0] == '&'
      type = 'conjunction'
      name = module_name[1..]
    else if module_name == 'broadcaster'
      type = 'broadcaster'

    modules[name] = {
      type: type
      destinations: destinations
      state: false  # For flip-flops
      memory: {}   # For conjunctions
    }
  modules

# Initialize conjunction module memories
initializeConjunctions = (modules) ->
  for name, module of modules
    for dest in module.destinations
      if modules[dest] and modules[dest].type == 'conjunction'
        modules[dest].memory[name] = false # Default to low pulse

# Simulate a single button press
pushTheButton = (modules) ->
  low_pulses = 0
  high_pulses = 0
  queue = [{ source: 'button', destination: 'broadcaster', pulse: false }] # Button sends low pulse to broadcaster

  while queue.length > 0
    {source, destination, pulse} = queue.shift()
    if pulse
      high_pulses += 1
    else
      low_pulses += 1

    module = modules[destination]
    continue unless module # Module might not exist

    if module.type == 'broadcaster'
      for dest in module.destinations
        queue.push { source: destination, destination: dest, pulse: pulse }

    else if module.type == 'flipflop'
      if not pulse # Only process low pulses
        module.state = not module.state # Flip state
        new_pulse = module.state
        for dest in module.destinations
          queue.push { source: destination, destination: dest, pulse: new_pulse }

    else if module.type == 'conjunction'
      module.memory[source] = pulse
      all_high = true
      for input_module, last_pulse of module.memory
        if not last_pulse
          all_high = false
          break

      new_pulse = not all_high
      for dest in module.destinations
        queue.push { source: destination, destination: dest, pulse: new_pulse }

  [low_pulses, high_pulses]

# Main function to solve the problem
solve = (filename, num_presses) ->
  modules = parseInput(filename)
  initializeConjunctions(modules)

  total_low_pulses = 0
  total_high_pulses = 0

  for _ in [1..num_presses]
    [low_pulses, high_pulses] = pushTheButton(modules)
    total_low_pulses += low_pulses
    total_high_pulses += high_pulses

  console.log "Low pulses: #{total_low_pulses}"
  console.log "High pulses: #{total_high_pulses}"
  console.log "Product: #{total_low_pulses * total_high_pulses}"

# Example usage
solve('input.txt', 1000)
