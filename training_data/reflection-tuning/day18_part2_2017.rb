class Program
  attr_reader :id, :registers, :queue, :pc, :send_count

  def initialize(id, instructions)
    @id = id
    @registers = Hash.new(0)
    @registers['p'] = id
    @queue = []
    @instructions = instructions
    @pc = 0
    @send_count = 0
    @waiting = false
  end

  def run(other_program)
    return if @waiting && @queue.empty?
    @waiting = false
    instruction = @instructions[@pc]
    case instruction[0]
    when 'snd'
      value = get_value(instruction[1])
      other_program.queue << value
      @send_count += 1 if @id == 1
    when 'set'
      @registers[instruction[1]] = get_value(instruction[2])
    when 'add'
      @registers[instruction[1]] += get_value(instruction[2])
    when 'mul'
      @registers[instruction[1]] *= get_value(instruction[2])
    when 'mod'
      @registers[instruction[1]] %= get_value(instruction[2])
    when 'rcv'
      if @queue.empty?
        @waiting = true
        return
      end
      @registers[instruction[1]] = @queue.shift
    when 'jgz'
      @pc += get_value(instruction[2]) - 1 if get_value(instruction[1]) > 0
    end
    @pc += 1
  end

  def get_value(x)
    x =~ /[a-z]/ ? @registers[x] : x.to_i
  end

  def deadlocked?(other_program)
    @waiting && other_program.waiting && @queue.empty? && other_program.queue.empty?
  end

  def waiting
    @waiting
  end
end

instructions = File.readlines('input.txt').map(&:split)
program0 = Program.new(0, instructions)
program1 = Program.new(1, instructions)

loop do
  program0.run(program1)
  program1.run(program0)
  break if program0.deadlocked?(program1)
end

puts program1.send_count
