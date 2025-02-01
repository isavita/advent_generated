
def solve_machine(m)
  d = m[:ax] * m[:by] - m[:ay] * m[:bx]
  return -1 if d == 0

  num_a = m[:px] * m[:by] - m[:py] * m[:bx]
  num_b = -m[:px] * m[:ay] + m[:py] * m[:ax]
  return -1 if num_a % d != 0 || num_b % d != 0

  a = num_a / d
  b = num_b / d
  return -1 if a < 0 || b < 0

  3 * a + b
end

def parse_val(s)
  s.delete!('XY=')
  s.to_i
end

def parse_line(s)
  x, y = s.split(',').map { |part| parse_val(part) }
  [x, y]
end

def parse_machine(lines)
  m = {}
  lines.each do |l|
    l.gsub!('Button A:', 'A:')
    l.gsub!('Button B:', 'B:')
    l.gsub!('Prize:', 'P:')
    if l.start_with?('A:')
      m[:ax], m[:ay] = parse_line(l[2..])
    elsif l.start_with?('B:')
      m[:bx], m[:by] = parse_line(l[2..])
    elsif l.start_with?('P:')
      m[:px], m[:py] = parse_line(l[2..])
    end
  end
  m
end

def read_input(filename)
  machines = []
  lines = []
  File.foreach(filename) do |line|
    line.strip!
    if line.empty?
      machines << parse_machine(lines) unless lines.empty?
      lines = []
    else
      lines << line
    end
  end
  machines << parse_machine(lines) unless lines.empty?
  machines
end

offset = 10**13
machines = read_input('input.txt')
machines.each do |m|
  m[:px] += offset
  m[:py] += offset
end

results = []
machines.each do |m|
  cost = solve_machine(m)
  results << cost if cost >= 0
end

if results.empty?
  puts '0 0'
else
  count = results.size
  sum = results.sum
  puts "#{count} #{sum}"
end
