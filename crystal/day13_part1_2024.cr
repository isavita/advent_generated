
class Machine
  property ax, ay, bx, by, px, py : Int32

  def initialize
    @ax = @ay = @bx = @by = @px = @py = 0
  end
end

def solve_machine(m : Machine) : Int32
  min_cost = -1
  0.upto(100) do |a|
    0.upto(100) do |b|
      x = m.ax * a + m.bx * b
      y = m.ay * a + m.by * b
      if x == m.px && y == m.py
        cost = a * 3 + b
        if min_cost == -1 || cost < min_cost
          min_cost = cost
        end
      end
    end
  end
  min_cost
end

a_regex = /(?:Button )?A: X\+(\d+), Y\+(\d+)/
b_regex = /(?:Button )?B: X\+(\d+), Y\+(\d+)/
p_regex = /(?:Prize|P): X=(\d+), Y=(\d+)/

machine = Machine.new
found_a = found_b = found_p = false
lines_read = 0
solved_count = 0
total_cost = 0_i64

File.open("input.txt", "r") do |file|
  file.each_line do |raw_line|
    line = raw_line.strip
    if line.empty?
      if lines_read > 0 && found_a && found_b && found_p
        cost = solve_machine(machine)
        if cost != -1
          solved_count += 1
          total_cost += cost
        end
      end
      machine = Machine.new
      found_a = found_b = found_p = false
      lines_read = 0
      next
    end

    lines_read += 1

    if m = a_regex.match(line)
      machine.ax = m[1].to_i32
      machine.ay = m[2].to_i32
      found_a = true
    elsif m = b_regex.match(line)
      machine.bx = m[1].to_i32
      machine.by = m[2].to_i32
      found_b = true
    elsif m = p_regex.match(line)
      machine.px = m[1].to_i32
      machine.py = m[2].to_i32
      found_p = true
    end
  end

  # Process last block if file didn't end with a blank line
  if lines_read > 0 && found_a && found_b && found_p
    cost = solve_machine(machine)
    if cost != -1
      solved_count += 1
      total_cost += cost
    end
  end
end

puts "#{solved_count} #{total_cost}"
