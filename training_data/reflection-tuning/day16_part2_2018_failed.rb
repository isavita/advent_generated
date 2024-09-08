class Device
  OPERATIONS = {
    addr: ->(r, a, b, c) { r[c] = r[a] + r[b] },
    addi: ->(r, a, b, c) { r[c] = r[a] + b },
    mulr: ->(r, a, b, c) { r[c] = r[a] * r[b] },
    muli: ->(r, a, b, c) { r[c] = r[a] * b },
    banr: ->(r, a, b, c) { r[c] = r[a] & r[b] },
    bani: ->(r, a, b, c) { r[c] = r[a] & b },
    borr: ->(r, a, b, c) { r[c] = r[a] | r[b] },
    bori: ->(r, a, b, c) { r[c] = r[a] | b },
    setr: ->(r, a, b, c) { r[c] = r[a] },
    seti: ->(r, a, b, c) { r[c] = a },
    gtir: ->(r, a, b, c) { r[c] = a > r[b] ? 1 : 0 },
    gtri: ->(r, a, b, c) { r[c] = r[a] > b ? 1 : 0 },
    gtrr: ->(r, a, b, c) { r[c] = r[a] > r[b] ? 1 : 0 },
    eqir: ->(r, a, b, c) { r[c] = a == r[b] ? 1 : 0 },
    eqri: ->(r, a, b, c) { r[c] = r[a] == b ? 1 : 0 },
    eqrr: ->(r, a, b, c) { r[c] = r[a] == r[b] ? 1 : 0 }
  }

  def initialize(input)
    @samples, @program = parse_input(input)
    @opcode_map = {}
  }

  def part1
    @samples.count { |sample| possible_opcodes(sample).size >= 3 }
  end

  def part2
    deduce_opcodes
    execute_program
  end

  private

  def parse_input(input)
    samples, program = input.split("\n\n\n\n")
    [
      samples.split("\n\n").map { |sample| parse_sample(sample) },
      program.split("\n").map { |line| line.split.map(&:to_i) }
    ]
  end

  def parse_sample(sample)
    before, instruction, after = sample.split("\n")
    {
      before: eval(before.split(": ").last),
      instruction: instruction.split.map(&:to_i),
      after: eval(after.split(":  ").last)
    }
  end

  def possible_opcodes(sample)
    OPERATIONS.select do |_, op|
      registers = sample[:before].dup
      op.call(registers, *sample[:instruction][1..3])
      registers == sample[:after]
    end.keys
  end

  def deduce_opcodes
    possibilities = {}
    @samples.each do |sample|
      opcode = sample[:instruction][0]
      possibilities[opcode] ||= OPERATIONS.keys
      possibilities[opcode] &= possible_opcodes(sample)
    end

    while possibilities.any? { |_, v| v.size > 1 }
      determined = possibilities.select { |_, v| v.size == 1 }.values.flatten
      possibilities.each do |k, v|
        v.subtract(determined) if v.size > 1
      end
    end

    @opcode_map = possibilities.transform_values(&:first)
  end

  def execute_program
    registers = [0, 0, 0, 0]
    @program.each do |instruction|
      opcode, a, b, c = instruction
      OPERATIONS[@opcode_map[opcode]].call(registers, a, b, c)
    end
    registers[0]
  end
end

input = File.read('input.txt')
device = Device.new(input)
puts "Part One: #{device.part1}"
puts "Part Two: #{device.part2}"
