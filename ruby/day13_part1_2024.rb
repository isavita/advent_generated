
def solve_machine(m)
  min_cost = -1
  101.times do |a_count|
    101.times do |b_count|
      x = m[:ax] * a_count + m[:bx] * b_count
      y = m[:ay] * a_count + m[:by] * b_count
      if x == m[:px] && y == m[:py]
        cost = a_count * 3 + b_count
        min_cost = cost if min_cost == -1 || cost < min_cost
      end
    end
  end
  min_cost
end

machines = []
current_machine = {}
File.readlines("input.txt").each do |line|
  line = line.strip
  if line.empty?
    machines << current_machine unless current_machine.empty?
    current_machine = {}
    next
  end
  if line.start_with?("Button A:")
    parts = line[9..].split(",").map(&:strip)
    current_machine[:ax] = parts[0].sub("X+", "").sub("X=", "").to_i
    current_machine[:ay] = parts[1].sub("Y+", "").sub("Y=", "").to_i
  elsif line.start_with?("Button B:")
    parts = line[9..].split(",").map(&:strip)
    current_machine[:bx] = parts[0].sub("X+", "").sub("X=", "").to_i
    current_machine[:by] = parts[1].sub("Y+", "").sub("Y=", "").to_i
  elsif line.start_with?("Prize:")
    parts = line[6..].split(",").map(&:strip)
    current_machine[:px] = parts[0].sub("X=", "").to_i
    current_machine[:py] = parts[1].sub("Y=", "").to_i
  end
end
machines << current_machine unless current_machine.empty?

results = machines.map { |m| solve_machine(m) }.select { |cost| cost >= 0 }

if results.empty?
  puts "0 0"
else
  puts "#{results.size} #{results.sum}"
end
