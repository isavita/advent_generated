
function main()
  local register5 = 0
  local seen = {}
  local last_unique = 0

  while true do
    local register3 = register5 | 65536
    register5 = 7586220

    while true do
      local register1 = register3 % 256
      register5 = (((register5 + register1) % 16777216) * 65899) % 16777216

      if register3 < 256 then
        if seen[register5] then
          print("Part Two Answer: " .. last_unique)
          return
        end
        seen[register5] = true
        last_unique = register5
        if #seen == 1 then
           print("Part One Answer: " .. register5)
        end
        break
      else
        register3 = math.floor(register3 / 256)
      end
    end
  end
end

main()
