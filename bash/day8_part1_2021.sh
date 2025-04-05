
#!/bin/bash

main() {
  awk '
    BEGIN { count = 0 }
    {
      n = split($2, digits, " ")
      for (i = 1; i <= n; i++) {
        len = length(digits[i])
        if (len == 2 || len == 3 || len == 4 || len == 7) {
          count++
        }
      }
    }
    END { print count }
  ' FS=' \\| ' input.txt
}

main
