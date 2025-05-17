
use v6; # Use the latest Raku language version

# Define the main entry point for the script
sub MAIN {
    # Read the entire content of the input file
    my $input = slurp 'input.txt';

    # Remove all whitespace characters from the input string
    # The :g adverb ensures global replacement (all occurrences)
    my $cleaned-input = $input.subst(/\s+/, '', :g);

    # Initialize the total decompressed length
    my $decompressed-length = 0;

    # Initialize an index to track the current position in the cleaned input string
    my $i = 0;

    # Iterate through the cleaned input string
    while $i < $cleaned-input.chars {
        # Try to match the marker pattern '(LxR)' starting at the current index $i
        # \(   - Match a literal opening parenthesis
        # (\d+) - Capture one or more digits (this will be L)
        # x    - Match a literal 'x'
        # (\d+) - Capture one or more digits (this will be R)
        # \)   - Match a literal closing parenthesis
        # :pos($i) - Start the match attempt at index $i
        my $match = $cleaned-input.match(/\((\d+)x(\d+)\)/, :pos($i));

        # Check if a marker was found at the current position
        if $match {
            # Extract the captured length (L) and repetition count (R) from the match
            # $match[0] is the first capture group (\d+) for L
            # $match[1] is the second capture group (\d+) for R
            # .Int converts the captured string to an integer
            my $L = $match[0].Int;
            my $R = $match[1].Int;

            # Add the contribution of this marker to the total length
            # The length added is L * R
            $decompressed-length += $L * $R;

            # Advance the index:
            # $match.to is the index *after* the end of the matched marker string
            # We then skip the next $L characters of the input string, as they were processed by the marker
            # The next position to check for a new marker or literal is after these L characters
            $i = $match.to + $L;

        } else {
            # No marker was found at the current position, so the character is a literal
            # Add 1 to the decompressed length for this literal character
            $decompressed-length += 1;

            # Move to the next character in the input string
            $i++;
        }
    }

    # Print the final calculated decompressed length
    say $decompressed-length;
}
