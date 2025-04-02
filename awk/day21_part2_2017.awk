
#!/usr/bin/awk -f

function rotate(input_str,   parts, size, new_parts, x, y, new_row) {
  split(input_str, parts, "/")
  size = length(parts)
  for (x = 1; x <= size; x++) {
    new_row = ""
    for (y = size; y >= 1; y--) {
      new_row = new_row substr(parts[y], x, 1)
    }
    new_parts[x] = new_row
  }
  return join(new_parts, "/")
}

function flip(input_str,   parts, i) {
  split(input_str, parts, "/")
  for (i = 1; i <= length(parts); i++) {
    parts[i] = reverse(parts[i])
  }
  return join(parts, "/")
}

function reverse(str,   i, result) {
  result = ""
  for (i = length(str); i >= 1; i--) {
    result = result substr(str, i, 1)
  }
  return result
}

function join(arr, sep,   i, str) {
  str = ""
  for (i = 1; i <= length(arr); i++) {
    str = str arr[i] (i == length(arr) ? "" : sep)
  }
  return str
}

function enhance(input_str, rules,   original, i, output) {
  original = input_str
  for (i = 0; i < 4; i++) {
    if (input_str in rules) {
      return rules[input_str]
    }
    input_str = rotate(input_str)
  }
  input_str = flip(input_str)
  for (i = 0; i < 4; i++) {
    if (input_str in rules) {
      return rules[input_str]
    }
    input_str = rotate(input_str)
  }
  return ""
}

function solve(   rules, grid, line, parts, i, sub_size, new_size, y, x, dy, new_grid, square, row, count, pixel) {
  grid[1] = ".#."
  grid[2] = "..#"
  grid[3] = "###"

  while ((getline line < "input.txt") > 0) {
    split(line, parts, " => ")
    rules[parts[1]] = parts[2]
  }
  close("input.txt")

  for (i = 0; i < 18; i++) {
    if (length(grid) % 2 == 0) {
      sub_size = 2
      new_size = length(grid) / 2 * 3
    } else {
      sub_size = 3
      new_size = length(grid) / 3 * 4
    }

    delete new_grid
    for (y = 1; y <= length(grid); y += sub_size) {
      for (x = 1; x <= length(grid); x += sub_size) {
        delete square
        for (dy = 0; dy < sub_size; dy++) {
          square[dy + 1] = substr(grid[y + dy], x, sub_size)
        }
        new_square = enhance(join(square, "/"), rules)
        split(new_square, parts, "/")
        for (dy = 1; dy <= length(parts); dy++) {
          new_grid[int((y - 1) / sub_size * (sub_size + 1) + dy)] = new_grid[int((y - 1) / sub_size * (sub_size + 1) + dy)] parts[dy]
        }
      }
    }
    delete grid
    for (y in new_grid) {
      grid[y] = new_grid[y]
    }
  }

  count = 0
  for (row in grid) {
    for (pixel = 1; pixel <= length(grid[row]); pixel++) {
      if (substr(grid[row], pixel, 1) == "#") {
        count++
      }
    }
  }
  print count
}

BEGIN {
  solve()
  exit
}
