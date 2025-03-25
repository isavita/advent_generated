
local function can_make(design, patterns)
  local n = string.len(design)
  local dp = {}
  dp[0] = true
  for i = 1, n do
    dp[i] = false
    for _, p in ipairs(patterns) do
      local lp = string.len(p)
      if i >= lp and dp[i - lp] and string.sub(design, i - lp + 1, i) == p then
        dp[i] = true
        break
      end
    end
  end
  return dp[n]
end

local function main()
  local file = io.open("input.txt", "r")
  local patterns_line = file:read("*l")
  local patterns = {}
  for pattern in string.gmatch(patterns_line, "([^,]+)") do
    table.insert(patterns, string.match(pattern, "^%s*(.-)%s*$"))
  end
  file:read("*l")
  local count = 0
  for design in file:lines() do
    if can_make(design, patterns) then
      count = count + 1
    end
  end
  file:close()
  print(count)
end

main()
