
sub MAIN {
    my %distances;
    for 'input.txt'.IO.lines -> $line {
        if $line ~~ m/^ (\w+) ' to ' (\w+) ' = ' (\d+) $/ {
            %distances{$0}{$1} = %distances{$1}{$0} = $2.Int;
        }
    }

    my @locations = (%distances.keys, %distances.values.map(*.keys).flat).flat.unique;
    my $max-distance = 0;

    for @locations.permutations -> @route {
        my $current-distance = ([+] @route.rotor(2 => -1).map: { %distances{$_[0]}{$_[1]} });
        $max-distance = max($max-distance, $current-distance);
    }

    say $max-distance;
}
