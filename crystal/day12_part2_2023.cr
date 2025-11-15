
class Row
  property springs : String
  property groups : Array(Int32)

  def initialize(@springs, @groups)
  end
end

def parse_row(line)
  parts = line.split
  springs = parts[0]
  groups = parts[1].split(',').map(&.to_i)
  Row.new(springs, groups)
end

def unfold_row(row, factor)
  new_springs = Array(String).new(factor) { row.springs }.join('?')
  new_groups = Array(Array(Int32)).new(factor) { row.groups }.flatten
  Row.new(new_springs, new_groups)
end

cache = Hash(Tuple(Int32, Int32, Int32), Int64).new

def count(row, si, gi, cur, cache)
  key = {si, gi, cur}
  cache[key] ||= begin
    if si == row.springs.size
      return 1_i64 if gi == row.groups.size && cur == 0
      return 1_i64 if gi == row.groups.size - 1 && cur == row.groups[gi]
      return 0_i64
    end

    res = 0_i64
    ch = row.springs[si]
    if ch.in?(['.', '?'])
      if cur == 0
        res += count(row, si + 1, gi, 0, cache)
      elsif gi < row.groups.size && cur == row.groups[gi]
        res += count(row, si + 1, gi + 1, 0, cache)
      end
    end
    if ch.in?(['#', '?'])
      if gi < row.groups.size && cur < row.groups[gi]
        res += count(row, si + 1, gi, cur + 1, cache)
      end
    end
    res
  end
end

total = 0_i64
File.read_lines("input.txt").each do |line|
  row = parse_row(line)
  unfolded = unfold_row(row, 5)
  cache.clear
  total += count(unfolded, 0, 0, 0, cache)
end
puts total
