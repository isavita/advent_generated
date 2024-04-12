fs = require 'fs'

# Converts string to int
atoi = (s) -> parseInt(s, 10)

# Reads the rules and constructs a map
readRules = (lines) ->
  rules = {}
  while lines.length > 0 and lines[0] != ''
    line = lines.shift()
    [num, rule] = line.split(': ')
    rules[atoi(num)] = rule.replace /"/g, ''
  rules

# Constructs a regex pattern from the rules
constructPattern = (rules, index) ->
  rule = rules[index]
  if '|' in rule
    subrules = rule.split(' | ')
    parts = (constructSubPattern(rules, sub) for sub in subrules)
    "(#{parts.join('|')})"
  else
    constructSubPattern(rules, rule)

# Constructs a sub-pattern for part of a rule
constructSubPattern = (rules, subrule) ->
  if subrule in ['a', 'b']
    subrule
  else
    subIdxs = subrule.split(' ')
    pattern = (constructPattern(rules, atoi(idx)) for idx in subIdxs)
    pattern.join('')

# Counts the number of messages that match rule 0
countMatches = (lines, pattern) ->
  count = 0
  re = new RegExp("^#{pattern}$")
  for line in lines
    count++ if re.test(line)
  count

# Main execution
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err then throw err
  
  lines = data.trim().split('\n')
  rules = readRules(lines)
  pattern = constructPattern(rules, 0)
  count = countMatches(lines, pattern)

  console.log "The number of messages that completely match rule 0 is:", count