
def addr(a, b, c, reg)
  reg[c] = reg[a] + reg[b]
end

def addi(a, b, c, reg)
  reg[c] = reg[a] + b
end

def mulr(a, b, c, reg)
  reg[c] = reg[a] * reg[b]
end

def muli(a, b, c, reg)
  reg[c] = reg[a] * b
end

def banr(a, b, c, reg)
  reg[c] = reg[a] & reg[b]
end

def bani(a, b, c, reg)
  reg[c] = reg[a] & b
end

def borr(a, b, c, reg)
  reg[c] = reg[a] | reg[b]
end

def bori(a, b, c, reg)
  reg[c] = reg[a] | b
end

def setr(a, _, c, reg)
  reg[c] = reg[a]
end

def seti(a, _, c, reg)
  reg[c] = a
end

def gtir(a, b, c, reg)
  reg[c] = a > reg[b] ? 1 : 0
end

def gtri(a, b, c, reg)
  reg[c] = reg[a] > b ? 1 : 0
end

def gtrr(a, b, c, reg)
  reg[c] = reg[a] > reg[b] ? 1 : 0
end

def eqir(a, b, c, reg)
  reg[c] = a == reg[b] ? 1 : 0
end

def eqri(a, b, c, reg)
  reg[c] = reg[a] == b ? 1 : 0
end

def eqrr(a, b, c, reg)
  reg[c] = reg[a] == reg[b] ? 1 : 0
end

instructions = [method(:addr), method(:addi), method(:mulr), method(:muli), method(:banr), method(:bani), method(:borr), method(:bori), method(:setr), method(:seti), method(:gtir), method(:gtri), method(:gtrr), method(:eqir), method(:eqri), method(:eqrr)]

samples = File.read("input.txt").split("\n\n\n")[0].split("\n\n")

count = 0

samples.each do |sample|
  before, instruction, after = sample.split("\n")
  before = eval(before.gsub("Before: ", ""))
  after = eval(after.gsub("After:  ", ""))
  opcode, a, b, c = instruction.split.map(&:to_i)
  
  matching = instructions.count do |instr|
    reg = before.dup
    instr.call(a, b, c, reg)
    reg == after
  end
  
  count += 1 if matching >= 3
end

puts count
