local function read_first_row(filename)
  local file, err = io.open(filename, "r")
  if not file then
    error("Failed to open file: " .. filename .. ": " .. err)
  end
  local first_row = file:read("*l")
  file:close()
  return first_row
end

local function is_trap(left, center, right, row)
  local l = (left < 1 or left > #row) and "." or row:sub(left, left)
  local c = row:sub(center, center)
  local r = (right < 1 or right > #row) and "." or row:sub(right, right)
  return (l == "^" and c == "^" and r == ".") or
         (c == "^" and r == "^" and l == ".") or
         (l == "^" and c == "." and r == ".") or
         (r == "^" and c == "." and l == ".")
end

local function count_safe_tiles(first_row, total_rows)
  local current_row = first_row
  local safe_count = 0
  for _ = 1, total_rows do
    safe_count = safe_count + (current_row:gsub("[^.]", ""):len())
    local next_row = ""
    for i = 1, #current_row do
      if is_trap(i - 1, i, i + 1, current_row) then
        next_row = next_row .. "^"
      else
        next_row = next_row .. "."
      end
    end
    current_row = next_row
  end
  return safe_count
end

local filename = "input.txt"
local first_row, err = read_first_row(filename)
if not first_row then
  error("Error reading file: " .. err)
end
local safe_tiles_count = count_safe_tiles(first_row, 400000)
print(safe_tiles_count)