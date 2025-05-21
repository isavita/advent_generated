
sub MAIN {
    my @card-points;
    my @card-counts;

    for 'input.txt'.IO.lines -> $line {
        next unless $line.chars;

        my ($numbers-str) = $line.split(': ', 2)[1];
        my ($winnings-str, $givens-str) = $numbers-str.split(' | ', 2);

        my $winnings = ($winnings-str.match(/\d+/, :g)).map(*.Int).Set;
        my $givens = ($givens-str.match(/\d+/, :g)).map(*.Int).Set;

        @card-points.push(($winnings ∩ $givens).elems);
        @card-counts.push(1);
    }

    for ^@card-points.elems -> $i {
        my $current-card-count = @card-counts[$i];
        my $points = @card-points[$i];

        for 1 .. $points -> $j {
            @card-counts[$i + $j] += $current-card-count;
        }
    }

    @card-counts.sum.say;
}
