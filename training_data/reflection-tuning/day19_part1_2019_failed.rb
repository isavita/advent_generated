require_relative 'intcode'  # Assuming we have an Intcode interpreter

def count_affected_points(program)
  affected_count = 0
  
  (0..49).each do |y|
    (0..49).each do |x|
      computer = Intcode.new(program.dup)
      computer.input(x)
      computer.input(y)
      output = computer.run
      affected_count += 1 if output == 1
    end
  end
  
  affected_count
end

# Read the Intcode program from file
program = File.read('day19_input.txt').strip.split(',').map(&:to_i)

result = count_affected_points(program)
puts "Number of points affected by the tractor beam: #{result}"
