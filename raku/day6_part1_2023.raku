
sub number-of-ways-to-win(Int $time, Int $record) {
    my $discriminant = $time**2 - 4 * $record;
    return 0 if $discriminant < 0;

    my $sqrt-discriminant = $discriminant.sqrt;

    my $lower-bound = ($time - $sqrt-discriminant) / 2;
    my $upper-bound = ($time + $sqrt-discriminant) / 2;

    my $first-winning-holdtime = ($lower-bound.floor + 1).Int;
    my $last-winning-holdtime = ($upper-bound.ceiling - 1).Int;

    $first-winning-holdtime = max(1, $first-winning-holdtime);
    $last-winning-holdtime = min($time - 1, $last-winning-holdtime);

    return 0 if $first-winning-holdtime > $last-winning-holdtime;

    $last-winning-holdtime - $first-winning-holdtime + 1;
}

sub calculate-ways(@times, @distances) {
    my $total-ways = 1;
    for @times Z @distances -> ($time, $distance) {
        $total-ways *= number-of-ways-to-win($time, $distance);
    }
    $total-ways;
}

sub main {
    my $lines = 'input.txt'.IO.slurp.lines;

    my @times = $lines[0].split(':')[1].words.map: *.Int;
    my @distances = $lines[1].split(':')[1].words.map: *.Int;

    calculate-ways(@times, @distances).say;
}

main;
