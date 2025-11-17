
lines = File.read_lines("input.txt")

samples = [] of Tuple(Array(Int32), Array(Int32), Array(Int32))

i = 0
while i < lines.size
  if lines[i].starts_with?("Before:")
    before = lines[i][9..-2].split(", ").map(&.to_i)
    instruction = lines[i+1].split.map(&.to_i)
    after = lines[i+2][9..-2].split(", ").map(&.to_i)
    samples << {before, instruction, after}
    i += 4
  else
    i += 1
  end
end

module Ops
  def self.addr(reg, a, b, c)
    reg[c] = reg[a] + reg[b]
  end

  def self.addi(reg, a, b, c)
    reg[c] = reg[a] + b
  end

  def self.mulr(reg, a, b, c)
    reg[c] = reg[a] * reg[b]
  end

  def self.muli(reg, a, b, c)
    reg[c] = reg[a] * b
  end

  def self.banr(reg, a, b, c)
    reg[c] = reg[a] & reg[b]
  end

  def self.bani(reg, a, b, c)
    reg[c] = reg[a] & b
  end

  def self.borr(reg, a, b, c)
    reg[c] = reg[a] | reg[b]
  end

  def self.bori(reg, a, b, c)
    reg[c] = reg[a] | b
  end

  def self.setr(reg, a, b, c)
    reg[c] = reg[a]
  end

  def self.seti(reg, a, b, c)
    reg[c] = a
  end

  def self.gtir(reg, a, b, c)
    reg[c] = a > reg[b] ? 1 : 0
  end

  def self.gtri(reg, a, b, c)
    reg[c] = reg[a] > b ? 1 : 0
  end

  def self.gtrr(reg, a, b, c)
    reg[c] = reg[a] > reg[b] ? 1 : 0
  end

  def self.eqir(reg, a, b, c)
    reg[c] = a == reg[b] ? 1 : 0
  end

  def self.eqri(reg, a, b, c)
    reg[c] = reg[a] == b ? 1 : 0
  end

  def self.eqrr(reg, a, b, c)
    reg[c] = reg[a] == reg[b] ? 1 : 0
  end
end

operations = [
  ->Ops.addr(Array(Int32), Int32, Int32, Int32),
  ->Ops.addi(Array(Int32), Int32, Int32, Int32),
  ->Ops.mulr(Array(Int32), Int32, Int32, Int32),
  ->Ops.muli(Array(Int32), Int32, Int32, Int32),
  ->Ops.banr(Array(Int32), Int32, Int32, Int32),
  ->Ops.bani(Array(Int32), Int32, Int32, Int32),
  ->Ops.borr(Array(Int32), Int32, Int32, Int32),
  ->Ops.bori(Array(Int32), Int32, Int32, Int32),
  ->Ops.setr(Array(Int32), Int32, Int32, Int32),
  ->Ops.seti(Array(Int32), Int32, Int32, Int32),
  ->Ops.gtir(Array(Int32), Int32, Int32, Int32),
  ->Ops.gtri(Array(Int32), Int32, Int32, Int32),
  ->Ops.gtrr(Array(Int32), Int32, Int32, Int32),
  ->Ops.eqir(Array(Int32), Int32, Int32, Int32),
  ->Ops.eqri(Array(Int32), Int32, Int32, Int32),
  ->Ops.eqrr(Array(Int32), Int32, Int32, Int32),
]

count = 0

samples.each do |(before, instruction, after)|
  a = instruction[1]
  b = instruction[2]
  c = instruction[3]
  
  matches = 0
  operations.each do |op|
    reg = before.dup
    op.call(reg, a, b, c)
    if reg == after
      matches += 1
      if matches >= 3
        count += 1
        break
      end
    end
  end
end

puts count
