
#!/bin/bash

main() {
  awk '
    BEGIN {
      RS = ""  # Process paragraph by paragraph
      FS = "\n" # Split paragraphs into lines
      error_rate = 0
      num_ranges = 0
    }

    # Block 1: Rules
    NR==1 {
      for (i = 1; i <= NF; ++i) {
        line = $i
        sub(/[^:]+: /, "", line) # Remove field name part
        gsub(/ or /, "\n", line) # Separate ranges by newline
        m = split(line, ranges_text, "\n")
        for (j = 1; j <= m; ++j) {
          split(ranges_text[j], se, "-")
          range_start[num_ranges] = se[1]
          range_end[num_ranges] = se[2]
          num_ranges++
        }
      }
      next
    }

    # Block 2: My Ticket (skip)
    NR==2 { next }

    # Block 3: Nearby Tickets
    NR==3 {
      # Start from line 2 (index 2) to skip header "nearby tickets:"
      for (i = 2; i <= NF; ++i) {
        m = split($i, values, ",")
        for (j = 1; j <= m; ++j) {
          value = values[j] + 0 # Force numeric
          is_valid_value = 0
          for (k = 0; k < num_ranges; ++k) {
            if (value >= range_start[k] && value <= range_end[k]) {
              is_valid_value = 1
              break
            }
          }
          if (!is_valid_value) {
            error_rate += value
          }
        }
      }
      print error_rate
    }
  ' < "input.txt"
}

main
