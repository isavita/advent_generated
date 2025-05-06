-- Read entire input file
local f = io.open("input.txt", "r")
local s = f:read("*all")
f:close()

local enabled = true
local sum = 0
local i = 1
local len = #s

while i <= len do
  -- find next occurrences
  local do_s, do_e = s:find("do%(%)", i)
  local dont_s, dont_e = s:find("don't%(%)", i)
  local mul_s, mul_e, a, b = s:find("mul%((%d+),(%d+)%)", i)

  -- choose earliest match
  local next_s = nil
  if do_s then next_s = do_s end
  if dont_s and (not next_s or dont_s < next_s) then next_s = dont_s end
  if mul_s and (not next_s or mul_s < next_s) then next_s = mul_s end
  if not next_s then break end

  -- process the match at next_s
  if mul_s == next_s then
    if enabled then
      sum = sum + (tonumber(a) * tonumber(b))
    end
    i = mul_e + 1

  elseif do_s == next_s then
    enabled = true
    i = do_e + 1

  elseif dont_s == next_s then
    enabled = false
    i = dont_e + 1
  end
end

print(sum)
