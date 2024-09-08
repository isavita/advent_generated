class CPU
  attr_accessor :registers

  def initialize(registers)
    @registers = registers
  end

  def addr(a, b, c); @registers[c] = @registers[a] + @registers[b]; end
  def addi(a, b, c); @registers[c] = @registers[a] + b; end
  def mulr(a, b, c); @registers[c] = @registers[a] * @registers[b]; end
  def muli(a, b, c); @registers[c] = @registers[a] * b; end
  def banr(a, b, c); @registers[c] = @registers[a] & @registers[b]; end
  def bani(a, b, c); @registers[c] = @registers[a] & b; end
  def borr(a, b, c); @registers[c] = @registers[a] | @registers[b]; end
  def bori(a, b, c); @registers[c] = @registers[a] | b; end
  def setr(a, b, c); @registers[c] = @registers[a]; end
  def seti(a, b, c); @registers[c] = a; end
  def gtir(a, b, c); @registers[c] = a > @registers[b] ? 1 : 0; end
  def gtri(a, b, c); @registers[c] = @registers[a] > b ? 1 : 0; end
  def gtrr(a, b, c); @registers[c] = @registers[a] > @registers[b] ? 1 : 0; end
  def eqir(a, b, c); @registers[c] = a == @registers[b] ? 1 : 0; end
  def eqri(a, b, c); @registers[c] = @registers[a] == b ? 1 : 0; end
  def eqrr(a, b, c); @registers[c] = @registers[a] == @registers[b] ? 1 : 0; end

  def opcodes
    %i[addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr]
  end

  def test_sample(before, instruction, after)
    _, a, b, c = instruction
    matching_opcodes = opcodes.count do |opcode|
      @registers = before.dup
      send(opcode, a, b, c)
      @registers == after
    end
    matching_opcodes >= 3
  end
end

samples = []
File.foreach('input.txt').each_slice(4) do |lines|
  break if lines[0].strip.empty?
  before = eval(lines[0].split(': ')[1])
  instruction = lines[1].split.map(&:to_i)
  after = eval(lines[2].split(':  ')[1])
  samples << [before, instruction, after]
end

cpu = CPU.new([])
result = samples.count { |sample| cpu.test_sample(*sample) }
puts result
