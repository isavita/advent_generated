require "file_utils"

def spin(programs, x)
  n = programs.size
  temp = programs.clone
  n.times do |i|
    programs[(i + x) % n] = temp[i]
  end
end

def exchange(programs, a, b)
  programs[a], programs[b] = programs[b], programs[a]
end

def partner(programs, a, b)
  index_a, index_b = -1, -1
  programs.each_with_index do |p, i|
    index_a = i if p == a
    index_b = i if p == b
  end
  exchange(programs, index_a, index_b)
end

file = File.read("input.txt")
moves = file.split(",")

programs = "abcdefghijklmnop".chars.to_a
initial = programs.join
cycle_len = 0

1000000000.times do |i|
  moves.each do |move|
    case move[0]
    when 's'
      x = move[1..].to_i
      spin(programs, x)
    when 'x'
      a, b = move[1..].split("/").map &.to_i
      exchange(programs, a, b)
    when 'p'
      a, b = move[1..].split("/").map &.chars.first
      partner(programs, a, b)
    end
  end

  if programs.join == initial
    cycle_len = i + 1
    break
  end
end

programs = initial.chars.to_a

(1000000000 % cycle_len).times do
  moves.each do |move|
    case move[0]
    when 's'
      x = move[1..].to_i
      spin(programs, x)
    when 'x'
      a, b = move[1..].split("/").map &.to_i
      exchange(programs, a, b)
    when 'p'
      a, b = move[1..].split("/").map &.chars.first
      partner(programs, a, b)
    end
  end
end

puts programs.join