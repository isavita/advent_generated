
#!/bin/bash

# Main function to process the light instructions
main() {
  awk '
    # Function to process a range based on action
    function process_range(x1, y1, x2, y2, action) {
      for (x = x1; x <= x2; x++) {
        for (y = y1; y <= y2; y++) {
          key = x SUBSEP y
          current_val = lights[key] # Defaults to 0 if not set

          if (action == 1) { # turn on
            lights[key] = current_val + 1
          } else if (action == -1) { # turn off
            lights[key] = current_val - 1
            if (lights[key] < 0) {
              lights[key] = 0
            }
          } else if (action == 2) { # toggle
            lights[key] = current_val + 2
          }
        }
      }
    }

    # Main processing block for each line
    {
      if ($1 == "turn") {
        action_type = $2 # "on" or "off"
        start_str = $3
        end_str = $5
        action_val = (action_type == "on") ? 1 : -1
      } else if ($1 == "toggle") {
        start_str = $2
        end_str = $4
        action_val = 2
      } else {
         next # Skip malformed lines
      }

      # Split coordinates
      split(start_str, start, ",")
      split(end_str, end, ",")

      # Process the range
      process_range(start[1], start[2], end[1], end[2], action_val)
    }

    # After processing all lines
    END {
      total_brightness = 0
      for (key in lights) {
        total_brightness += lights[key]
      }
      print total_brightness
    }
  ' input.txt # Read input from input.txt
}

# Execute the main function
main
