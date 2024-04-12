fs = require 'fs'

opcodeFuncs =
  addr: (regs, a, b, c) -> regs[c] = regs[a] + regs[b]
  addi: (regs, a, b, c) -> regs[c] = regs[a] + b
  mulr: (regs, a, b, c) -> regs[c] = regs[a] * regs[b]
  muli: (regs, a, b, c) -> regs[c] = regs[a] * b
  banr: (regs, a, b, c) -> regs[c] = regs[a] & regs[b]
  bani: (regs, a, b, c) -> regs[c] = regs[a] & b
  borr: (regs, a, b, c) -> regs[c] = regs[a] | regs[b]
  bori: (regs, a, b, c) -> regs[c] = regs[a] | b
  setr: (regs, a, b, c) -> regs[c] = regs[a]
  seti: (regs, a, b, c) -> regs[c] = a
  gtir: (regs, a, b, c) -> regs[c] = if a > regs[b] then 1 else 0
  gtri: (regs, a, b, c) -> regs[c] = if regs[a] > b then 1 else 0
  gtrr: (regs, a, b, c) -> regs[c] = if regs[a] > regs[b] then 1 else 0
  eqir: (regs, a, b, c) -> regs[c] = if a == regs[b] then 1 else 0
  eqri: (regs, a, b, c) -> regs[c] = if regs[a] == b then 1 else 0
  eqrr: (regs, a, b, c) -> regs[c] = if regs[a] == regs[b] then 1 else 0

parseInput = (input) ->
  lines = input.trim().split '\n'
  ip = parseInt lines[0].split(' ')[1]
  instructions = (line.split(' ') for line in lines[1..])
  {ip, instructions}

tick = (computer) ->
  {ip, regs, instructions} = computer
  if ip >= instructions.length
    throw new Error 'Out of range instruction, terminating...'
  [name, a, b, c] = instructions[regs[ip]]
  a = parseInt a
  b = parseInt b
  c = parseInt c
  opcodeFuncs[name](regs, a, b, c)
  regs[ip]++
  done = (regs[ip] >= instructions.length)
  {done, regs}

solve = (input) ->
  {ip, instructions} = parseInput input
  regs = [0, 0, 0, 0, 0, 0]
  comparedRegister5s = {}
  lastReg5 = null
  computer = {ip, regs, instructions}
  done = false
  while not done
    {done, regs} = tick computer
    if regs[ip] == 28
      reg5 = regs[5]
      break if comparedRegister5s[reg5]
      comparedRegister5s[reg5] = true
      lastReg5 = reg5
  lastReg5

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  console.log solve(data)