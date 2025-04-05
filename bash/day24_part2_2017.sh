
#!/bin/bash

# Solution using awk for efficiency

main() {
  if [[ ! -f "input.txt" ]]; then
    echo "Error: input.txt not found." >&2
    exit 1
  fi

  awk '
    BEGIN {
      FS="/"
      max_len = 0
      max_strength_at_max_len = 0
      n = 0
    }

    # Store components
    {
      n++
      port1[n] = $1 + 0 # Ensure numeric conversion
      port2[n] = $2 + 0 # Ensure numeric conversion
    }

    # Recursive function to build bridges
    function build_bridge(current_port, current_strength, current_length,   i, next_p, comp_strength) {
      # Update global maximums
      if (current_length > max_len) {
        max_len = current_length
        max_strength_at_max_len = current_strength
      } else if (current_length == max_len && current_strength > max_strength_at_max_len) {
        max_strength_at_max_len = current_strength
      }

      # Try adding unused components
      for (i = 1; i <= n; i++) {
        if (!used[i]) {
          next_p = -1 # Sentinel value
          if (port1[i] == current_port) {
            next_p = port2[i]
          } else if (port2[i] == current_port) {
            next_p = port1[i]
          }

          if (next_p != -1) {
            used[i] = 1 # Mark as used
            comp_strength = port1[i] + port2[i]
            build_bridge(next_p, current_strength + comp_strength, current_length + 1)
            used[i] = 0 # Backtrack: mark as unused for other branches
          }
        }
      }
    }

    END {
      # Start the search from port 0
      build_bridge(0, 0, 0)
      # Print the final result
      print max_strength_at_max_len
    }
  ' input.txt
}

main
