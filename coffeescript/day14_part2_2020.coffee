
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

# Part 1
memory1 = {}
mask = null

for line in input
  if line.startsWith('mask')
    mask = line.split(' = ')[1]
  else
    match = line.match(/mem\[(\d+)\] = (\d+)/)
    address = parseInt(match[1])
    value = parseInt(match[2])
    
    binaryValue = value.toString(2).padStart(36, '0')
    maskedValue = ''
    for i in [0...36]
      if mask[i] == 'X'
        maskedValue += binaryValue[i]
      else
        maskedValue += mask[i]
    
    memory1[address] = parseInt(maskedValue, 2)

sum1 = 0
for address, value of memory1
  sum1 += value

console.log "Part 1 Sum:", sum1

# Part 2
memory2 = {}
mask = null

generateAddresses = (maskedAddress) ->
  addresses = []
  xIndices = []
  for i in [0...maskedAddress.length]
    if maskedAddress[i] == 'X'
      xIndices.push(i)
  
  numX = xIndices.length
  for i in [0...2**numX]
    binary = i.toString(2).padStart(numX, '0')
    newAddress = maskedAddress
    for j in [0...numX]
      newAddress = newAddress.substring(0, xIndices[j]) + binary[j] + newAddress.substring(xIndices[j] + 1)
    addresses.push(parseInt(newAddress, 2))
  addresses

for line in input
  if line.startsWith('mask')
    mask = line.split(' = ')[1]
  else
    match = line.match(/mem\[(\d+)\] = (\d+)/)
    address = parseInt(match[1])
    value = parseInt(match[2])
    
    binaryAddress = address.toString(2).padStart(36, '0')
    maskedAddress = ''
    for i in [0...36]
      if mask[i] == '0'
        maskedAddress += binaryAddress[i]
      else
        maskedAddress += mask[i]
    
    addresses = generateAddresses(maskedAddress)
    for addr in addresses
      memory2[addr] = value

sum2 = 0
for address, value of memory2
  sum2 += value

console.log "Part 2 Sum:", sum2
