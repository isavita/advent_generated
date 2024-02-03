
Row = Struct.new(:springs, :group)

def parse_input(input)
  rows = []
  input.each do |line|
    parts = line.split(" ")
    springs = parts[0]
    ints = parse_string_to_ints(parts[1])

    row = Row.new(springs, ints)
    rows << row
  end
  rows
end

def parse_string_to_ints(numbers_line)
  numbers = []
  numbers_parts = numbers_line.split(",")
  numbers_parts.each do |number_str|
    number = number_str.to_i
    numbers << number
  end
  numbers
end

def count_arrangements_recursive(row, i_springs, i_group, i_contiguous_damaged, cache)
  if i_springs == row.springs.length
    if i_group == row.group.length && i_contiguous_damaged == 0
      return 1
    elsif i_group == row.group.length - 1 && i_contiguous_damaged == row.group[i_group]
      return 1
    end
    return 0
  end

  cache_key = [i_springs, i_group, i_contiguous_damaged]
  if cache.key?(cache_key)
    return cache[cache_key]
  end

  res = 0
  char = row.springs[i_springs]
  if char == '.' || char == '?'
    if i_contiguous_damaged == 0
      res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged, cache)
    elsif i_contiguous_damaged == row.group[i_group]
      res += count_arrangements_recursive(row, i_springs + 1, i_group + 1, 0, cache)
    end
  end
  if char == '#' || char == '?'
    if i_group < row.group.length && i_contiguous_damaged < row.group[i_group]
      res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged + 1, cache)
    end
  end

  cache[cache_key] = res
  res
end

def count_arrangements(row)
  count_arrangements_recursive(row, 0, 0, 0, {})
end

def unfold_row(row, unfolding_factor)
  new_row = Row.new(row.springs, row.group)

  (1...unfolding_factor).each do |i|
    new_row.springs += "?" + row.springs
    new_row.group += row.group
  end

  new_row
end

def solve(input)
  rows = parse_input(input)

  unfolded_rows = []
  rows.each do |row|
    unfolded_rows << unfold_row(row, 5)
  end

  res = 0
  unfolded_rows.each do |row|
    res += count_arrangements(row)
  end

  res
end

def read_file(file_name)
  file = File.read(file_name)
  file.split("\n")
end

input = read_file("input.txt")
puts solve(input)
