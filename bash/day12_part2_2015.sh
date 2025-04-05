
#!/bin/bash

# main entry point
main() {
    # Check if input file exists
    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    # Use jq to parse the JSON and calculate the sum
    # Define a recursive function 'sum_non_red' within jq:
    # - If the input is a number, return it.
    # - If it's an array, recursively call sum_non_red on each element and sum the results.
    # - If it's an object, check if any value is the string "red".
    #   - If "red" is found, return 0 for this object and its contents.
    #   - Otherwise, recursively call sum_non_red on each value and sum the results.
    # - For any other type (string, boolean, null), return 0.
    # Finally, call sum_non_red on the root element read from input.txt.
    jq '
      def sum_non_red:
        if type == "number" then .
        elif type == "array" then map(sum_non_red) | add
        elif type == "object" then
          if any(.[]; . == "red") then 0
          else map_values(sum_non_red) | add
          end
        else 0 # Other types contribute 0
        end;
      sum_non_red
    ' input.txt
}

# Execute the main function
main
