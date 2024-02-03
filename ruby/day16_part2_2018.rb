
def addr(a, b, c, regs)
  regs[c] = regs[a] + regs[b]
end

def addi(a, b, c, regs)
  regs[c] = regs[a] + b
end

def mulr(a, b, c, regs)
  regs[c] = regs[a] * regs[b]
end

def muli(a, b, c, regs)
  regs[c] = regs[a] * b
end

def banr(a, b, c, regs)
  regs[c] = regs[a] & regs[b]
end

def bani(a, b, c, regs)
  regs[c] = regs[a] & b
end

def borr(a, b, c, regs)
  regs[c] = regs[a] | regs[b]
end

def bori(a, b, c, regs)
  regs[c] = regs[a] | b
end

def setr(a, _, c, regs)
  regs[c] = regs[a]
end

def seti(a, _, c, regs)
  regs[c] = a
end

def gtir(a, b, c, regs)
  regs[c] = a > regs[b] ? 1 : 0
end

def gtri(a, b, c, regs)
  regs[c] = regs[a] > b ? 1 : 0
end

def gtrr(a, b, c, regs)
  regs[c] = regs[a] > regs[b] ? 1 : 0
end

def eqir(a, b, c, regs)
  regs[c] = a == regs[b] ? 1 : 0
end

def eqri(a, b, c, regs)
  regs[c] = regs[a] == b ? 1 : 0
end

def eqrr(a, b, c, regs)
  regs[c] = regs[a] == regs[b] ? 1 : 0
end

ops = [method(:addr), method(:addi), method(:mulr), method(:muli), method(:banr), method(:bani), method(:borr), method(:bori), method(:setr), method(:seti), method(:gtir), method(:gtri), method(:gtrr), method(:eqir), method(:eqri), method(:eqrr)]

input = File.read("input.txt").split("\n\n\n\n")
samples, program = input[0].split("\n\n"), input[1].split("\n")

matching = 0

samples.each do |sample|
  before, instruction, after = sample.split("\n")
  before = eval(before.split(": ")[1])
  after = eval(after.split(":  ")[1])
  opcode, a, b, c = instruction.split.map(&:to_i)

  matches = ops.count do |op|
    regs = before.dup
    op.call(a, b, c, regs)
    regs == after
  end

  matching += 1 if matches >= 3
end

puts matching

opcode_map = {}
until opcode_map.size == 16
  samples.each do |sample|
    before, instruction, after = sample.split("\n")
    before = eval(before.split(": ")[1])
    after = eval(after.split(":  ")[1])
    opcode, a, b, c = instruction.split.map(&:to_i)

    possible_ops = ops.select do |op|
      regs = before.dup
      op.call(a, b, c, regs)
      regs == after
    end

    possible_ops.reject! { |op| opcode_map.values.include?(op) }
    opcode_map[opcode] = possible_ops.first if possible_ops.size == 1
  end
end

regs = [0, 0, 0, 0]
program.each do |line|
  opcode, a, b, c = line.split.map(&:to_i)
  opcode_map[opcode].call(a, b, c, regs)
end

puts regs[0]
