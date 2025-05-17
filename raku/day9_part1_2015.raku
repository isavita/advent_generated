
#!/usr/bin/env raku

# Raku program to find the shortest route visiting all locations exactly once.
# Reads distances from input.txt and prints the minimum distance.

sub main {
    my %distances;  # Hash to store distances: %distances{location1}{location2} = distance
    my %locations;  # Hash to keep track of all unique locations

    # Define the input file name
    my $input-file = "input.txt";

    # Check if the input file exists
    unless $input-file.IO.e {
        say "Error: Input file '$input-file' not found.";
        exit;
    }

    # Read the input file line by line
    for $input-file.IO.lines -> $line {
        # Parse each line using a regular expression
        # Captures location names (allowing spaces) and the distance
        if $line ~~ m/(.+?) ' to ' (.+?) ' = ' (\d+)/ {
            # Extract captured values
            my ($loc1, $loc2, $dist) = $0, $1, $2.Int;

            # Store the distance in both directions since routes can be traversed either way
            %distances{$loc1}{$loc2} = $dist;
            %distances{$loc2}{$loc1} = $dist;

            # Add locations to our set of unique locations (using hash keys for uniqueness)
            %locations{$loc1} = True;
            %locations{$loc2} = True;
        } else {
            # Optional: Handle lines that don't match the expected format
            # warn "Skipping malformed line: $line" unless $line.trim.is-empty;
        }
    }

    # Get the list of all unique locations from the keys of %locations
    my @locations = %locations.keys;

    # Initialize minimum distance to positive infinity
    my $min_distance = Inf;

    # Generate all possible permutations of the locations (all possible routes)
    for @locations.permutations -> @route {
        my $current_distance = 0;

        # Calculate the total distance for the current route
        # Iterate through the route from the first location up to the second-to-last
        for 0 ..^ @route.end -> $i {
            my $loc1 = @route[$i];       # Current location
            my $loc2 = @route[$i + 1];   # Next location

            # Look up the distance between the current and next location
            # %distances{$loc1}{$loc2} is guaranteed to exist because we stored distances bidirectionally
            # and @route contains only keys found in %distances.
            $current_distance += %distances{$loc1}{$loc2};
        }

        # Update the minimum distance if the current route is shorter
        $min_distance min= $current_distance;
    }

    # Print the shortest distance found
    say $min_distance;
}

# Execute the main subroutine
main();
