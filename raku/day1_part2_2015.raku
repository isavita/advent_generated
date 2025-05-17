
use v6.c; # Use the latest stable Raku version features

# Entry point for the program
sub MAIN {
    my $input-file = 'input.txt';

    # Ensure the input file exists
    unless $input-file.IO.e {
        die "Error: Input file '$input-file' not found.\n";
    }

    # Read the entire content of the input file into a string
    my $input = slurp $input-file;

    my $floor = 0;
    my $basement-position = -1; # Initialize to -1, meaning basement not yet entered

    # Iterate through the characters of the input string with their 0-based index
    for $input.comb.kv -> $index, $char {
        # Update Santa's floor based on the character
        if $char eq '(' {
            $floor++;
        } elsif $char eq ')' {
            $floor--;
        }
        # else: Ignore any other characters if present (problem implies only () )

        # Part 2: Check if Santa has entered the basement (floor -1)
        # We only care about the *first* time this happens
        if $floor == -1 and $basement-position == -1 {
            # Store the 1-based position (index + 1)
            $basement-position = $index + 1;
            # We could technically 'last' here if only Part 2 was needed,
            # but we need the final floor for Part 1 as well.
            # Processing the rest of the string still contributes to the final floor.
        }
    }

    # After processing all characters:
    # $floor holds the final floor for Part 1.
    # $basement-position holds the position where the basement was first entered for Part 2.

    # Print the results to standard output
    say "Part 1: The final floor is $floor";
    say "Part 2: The position of the first character to enter the basement is $basement-position";
}
