
class Program
  attr_reader :send_count

  def initialize(id, instructions)
    @registers = Hash.new(0)
    @registers['p'] = id
    @instructions = instructions
    @pointer = 0
    @queue = []
    @send_count = 0
    @waiting = false
  end

  def execute(other)
    @waiting = false
    while @pointer >= 0 && @pointer < @instructions.length && !@waiting
      parts = @instructions[@pointer].split
      case parts[0]
      when 'snd'
        other.receive(evaluate(parts[1]))
        @send_count += 1
      when 'set'
        @registers[parts[1]] = evaluate(parts[2])
      when 'add'
        @registers[parts[1]] += evaluate(parts[2])
      when 'mul'
        @registers[parts[1]] *= evaluate(parts[2])
      when 'mod'
        @registers[parts[1]] %= evaluate(parts[2])
      when 'rcv'
        if @queue.empty?
          @waiting = true
        else
          @registers[parts[1]] = @queue.shift
        end
      when 'jgz'
        @pointer += evaluate(parts[2]) - 1 if evaluate(parts[1]) > 0
      end
      @pointer += 1 unless @waiting
    end
  end

  def receive(value)
    @queue << value
    @waiting = false
  end

  def waiting?
    @waiting
  end

  private

  def evaluate(x)
    x.match?(/[a-z]/) ? @registers[x] : x.to_i
  end
end

instructions = File.readlines('input.txt').map(&:chomp)
program0 = Program.new(0, instructions)
program1 = Program.new(1, instructions)

loop do
  program0.execute(program1)
  program1.execute(program0)
  break if program0.waiting? && program1.waiting?
end

puts program1.send_count
