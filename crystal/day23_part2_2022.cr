
dirs = [{-1, -1}, {-1, 0}, {-1, 1}, {0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}]
order = [1, 5, 7, 3]
curr_dir = 0
elves = [] of Tuple(Int32, Int32)
map = Set(Tuple(Int32, Int32)).new

File.read_lines("input.txt").each_with_index do |line, row|
  line.chars.each_with_index do |ch, col|
    if ch == '#'
      elves << {row, col}
      map.add({row, col})
    end
  end
end

(1..).each do |round|
  proposes = Hash(Tuple(Int32, Int32), Int32).new(0)
  moves = [] of Tuple(Tuple(Int32, Int32), Tuple(Int32, Int32))

  elves.each do |pos|
    x, y = pos
    next if dirs.all? { |dx, dy| !map.includes?({x + dx, y + dy}) }

    4.times do |j|
      dir = order[(curr_dir + j) % 4]
      next if (-1..1).any? { |k|
        d = (dir + k + 8) % 8
        dx, dy = dirs[d]
        map.includes?({x + dx, y + dy})
      }

      dx, dy = dirs[dir]
      next_pos = {x + dx, y + dy}
      proposes[next_pos] += 1
      moves << {pos, next_pos}
      break
    end
  end

  moved = false
  moves.each do |from, to|
    next if proposes[to] > 1
    moved = true
    map.delete(from)
    map.add(to)
    elves.map! { |e| e == from ? to : e }
  end

  unless moved
    puts round
    exit
  end

  curr_dir = (curr_dir + 1) % 4
end
