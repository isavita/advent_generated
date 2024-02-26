
# Read the input from the file "input.txt" and store it in the variable "input"
BEGIN {
    while (getline < "input.txt") {
        input = input $0;
    }

    # Perform 40 iterations of the look-and-say process
    for (i = 1; i <= 40; i++) {
        prevChar = substr(input, 1, 1);
        count = 1;
        output = "";

        # Process each character in the input string
        for (j = 2; j <= length(input); j++) {
            currChar = substr(input, j, 1);

            # If the current character is the same as the previous one, increment the count
            if (currChar == prevChar) {
                count++;
            } else {
                # Otherwise, append the count and the previous character to the output,
                # and update the previous character and count for the next iteration
                output = output count prevChar;
                count = 1;
                prevChar = currChar;
            }
        }

        # Append the count and the last character to the output after the loop
        output = output count prevChar;

        # Update the input string for the next iteration
        input = output;
    }

    # Print the length of the final string after 40 iterations
    print length(input);
}
