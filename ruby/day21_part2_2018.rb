def main
  register5 = 0
  seen = {}
  last_unique = 0

  loop do
    register3 = register5 | 0x10000  # 65536
    register5 = 7586220

    while true
      register1 = register3 & 0xFF    # 255
      register5 = (((register5 + register1) & 0xFFFFFF) * 65899) & 0xFFFFFF

      if register3 < 0x100  # 256
        if seen.key?(register5)
          puts last_unique
          return
        end
        seen[register5] = true
        last_unique = register5
        break
      else
        register3 = register3 / 256
      end
    end
  end
end

main
