
def parse_input(input : Array(String))
  rows = [] of {String, Array(Int32)}
  input.each do |line|
    parts = line.split(" ")
    springs = parts[0]
    ints = parts[1].split(",").map(&.to_i)
    rows << {springs, ints}
  end
  rows
end

def count_arrangements_recursive(row : Tuple(String, Array(Int32)), i_springs, i_group, i_contiguous_damaged, cache)
  if i_springs == row[0].size
    return 1 if i_group == row[1].size && i_contiguous_damaged == 0
    return 1 if i_group == row[1].size - 1 && i_contiguous_damaged == row[1][i_group]
    return 0
  end

  cache_key = {i_springs, i_group, i_contiguous_damaged}
  cached_value = cache[cache_key]?
  return cached_value.as(Int32) if cached_value

  res = 0
  char = row[0][i_springs]
  if char == '.' || char == '?'
    if i_contiguous_damaged == 0
      res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged, cache)
    elsif i_group < row[1].size && i_contiguous_damaged == row[1][i_group]
      res += count_arrangements_recursive(row, i_springs + 1, i_group + 1, 0, cache)
    end
  end
  if char == '#' || char == '?'
    if i_group < row[1].size && i_contiguous_damaged < row[1][i_group]
      res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged + 1, cache)
    end
  end

  cache[cache_key] = res
  res
end

def count_arrangements(row)
  count_arrangements_recursive(row, 0, 0, 0, {} of {Int32, Int32, Int32} => Int32)
end

def solve(input : Array(String))
  rows = parse_input(input)
  res = 0
  rows.each do |row|
    res += count_arrangements(row)
  end
  res
end

input = File.read_lines("input.txt")
puts solve(input)
