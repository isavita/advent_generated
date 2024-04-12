fs = require 'fs'

calculate = (monkey, jobs, results) ->
  return results[monkey] if results[monkey]?
  
  job = jobs[monkey]
  throw "Monkey not found: #{monkey}" unless job?
  
  if /^\d+$/.test(job)
    results[monkey] = parseInt(job)
    return results[monkey]
  
  parts = job.split ' '
  a = calculate(parts[0], jobs, results)
  b = calculate(parts[2], jobs, results)
  
  result = switch parts[1]
    when "+" then a + b
    when "-" then a - b
    when "*" then a * b
    when "/" then Math.floor(a / b)
    else throw "Unknown operation: #{parts[1]}"
  
  results[monkey] = result
  return result

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err?
  
  jobs = {}
  results = {}
  for line in data.trim().split('\n')
    [key, value] = line.split(': ')
    jobs[key] = value
  
  console.log calculate('root', jobs, results)