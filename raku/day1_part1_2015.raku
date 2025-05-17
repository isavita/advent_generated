#!/usr/bin/env raku

# Read the input file
my $input = "input.txt".IO.slurp;

# Calculate the floor
my $floor = 0;
for $input.comb -> $char {
    given $char {
        when '(' { $floor++ }
        when ')' { $floor-- }
        default  { } # Ignore other characters
    }
}

# Print the result
say "Final floor: $floor";
