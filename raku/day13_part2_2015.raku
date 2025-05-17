
use v6;

# Reads input from input.txt and calculates the optimal seating arrangement
# for maximizing total happiness, considering a circular table.
# Solves both Part 1 (original guest list) and Part 2 (adding yourself).

sub MAIN {
    my %happiness; # Stores happiness changes: %happiness<PersonA><PersonB> is A's change next to B
    my %people;    # Stores unique names of people involved

    # Read and parse the input file line by line
    for "input.txt".IO.lines -> $line {
        # Use a regex to extract the relevant parts of the line
        given $line.match(/^
            $<person1>=\w+ \s+ 'would' \s+
            $<gainlose>=(gain|lose) \s+
            $<amount>=\d+ \s+
            'happiness units by sitting next to' \s+
            $<person2>=\w+ \.\s*$ # Match person2, a period, optional space, and end of line
        $/) {
            if $_ {
                # Calculate the numerical happiness value (positive for gain, negative for lose)
                my $value = $<amount>.Int * ($<gainlose> eq 'lose' ?? -1 !! 1);
                # Store the relationship
                %happiness{$<person1>.Str}{$<person2>.Str} = $value;
                # Keep track of all unique people encountered
                %people{$<person1>.Str} = True;
                %people{$<person2>.Str} = True;
            } else {
                # Warn if a line doesn't match the expected format
                warn "Skipping unparseable line: $line";
            }
        }
    }

    # Get a list of unique people involved in the original input
    my @people = %people.keys;

    # Part 1: Calculate the maximum total happiness for the original group
    my $max_happiness_part1 = calculate-max-happiness(@people, %happiness);
    say "Part 1: {$max_happiness_part1}";

    # Part 2: Add yourself ('Me') to the group with zero happiness interactions
    # Create a new hash for Part 2's happiness data by copying the original
    # A shallow copy is sufficient here as we are only adding new top-level keys ('Me')
    # and new nested hashes under 'Me', not modifying existing nested hashes.
    my %happiness_part2 = %happiness;
    # Add 'Me' to the list of people for Part 2 calculations
    my @people_part2 = @people.push('Me');

    # Add happiness relationships involving 'Me'. All are 0.
    # Loop through the original people to set their relationship with 'Me' and vice-versa.
    for @people -> $person {
        %happiness_part2{$person}{'Me'} = 0;
        %happiness_part2{'Me'}{$person} = 0;
    }
    # Relationship of 'Me' with 'Me' is also 0 (not strictly necessary for neighbors, but complete)
    %happiness_part2{'Me'}{'Me'} = 0;

    # Calculate the maximum total happiness for the group including 'Me'
    my $max_happiness_part2 = calculate-max-happiness(@people_part2, %happiness_part2);
    say "Part 2: {$max_happiness_part2}";
}

# Helper subroutine to calculate the maximum total happiness for a circular
# seating arrangement given a list of people and their happiness relationships.
# It optimizes by fixing the position of the first person and permuting the rest,
# which explores all unique arrangements considering rotation and the symmetric
# nature of the happiness sum calculation (A->B + B->A).
sub calculate-max-happiness(@people, %happiness) {
    # If there are 0 or 1 person, there are no neighbors and thus 0 happiness.
    if @people.elems <= 1 {
        return 0;
    }

    my $max_total_happiness = -Inf; # Initialize with a value guaranteed to be lower than any possible happiness

    # To find the maximum happiness, we need to check all unique circular arrangements.
    # Fixing the position of one person eliminates rotations. Permuting the remaining
    # N-1 people explores all distinct sets of neighbors around the circle.
    my $pivot_person = @people[0];
    my @permute_people = @people[1 .. *]; # Get the list of people excluding the pivot

    # Iterate through all permutations of the remaining people
    for @permute_people.permutations -> @arrangement_tail {
        # Construct the full arrangement by placing the pivot person first
        my @arrangement = ($pivot_person, |@arrangement_tail);
        my $current_total_happiness = 0;
        my $n = @arrangement.elems;

        # Calculate the total happiness for this specific circular arrangement
        # We iterate through each person and their right neighbor in the circle
        for 0 ..^ $n -> $i {
            my $person1 = @arrangement[$i];
            # The neighbor to the right, calculated with wrap-around using modulo
            my $person2 = @arrangement[($i + 1) % $n];

            # The total happiness change for this pair of neighbors is the sum of
            # person1's happiness from person2 and person2's happiness from person1.
            # Use // 0 to handle potential missing keys defensively, though with
            # proper input parsing and data structure setup it should not be needed.
            $current_total_happiness += %happiness{$person1}{$person2} // 0;
            $current_total_happiness += %happiness{$person2}{$person1} // 0;
        }

        # Update the overall maximum happiness found if the current arrangement is better
        $max_total_happiness = max($max_total_happiness, $current_total_happiness);
    }

    return $max_total_happiness;
}
