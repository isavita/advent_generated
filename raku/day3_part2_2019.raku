
sub MAIN {
    my @lines = 'input.txt'.IO.lines;
    my %wire1 = get-points-with-steps(@lines[0]);
    my %wire2 = get-points-with-steps(@lines[1]);

    my $min-steps = Inf;

    for %wire1.kv -> $point, $steps1 {
        if %wire2{$point}:exists {
            my $steps2 = %wire2{$point};
            my $total-steps = $steps1 + $steps2;
            $min-steps = min($min-steps, $total-steps);
        }
    }

    print $min-steps;
}

sub get-points-with-steps(Str $path) {
    my %points;
    my $current = (0, 0);
    my $steps = 0;

    for $path.split(',') -> $move {
        my $direction = $move.substr(0, 1);
        my $distance = $move.substr(1).Int;

        for 1..$distance {
            $steps++;
            if $direction eq 'U' {
                $current = ($current[0], $current[1] + 1);
            } elsif $direction eq 'D' {
                $current = ($current[0], $current[1] - 1);
            } elsif $direction eq 'L' {
                $current = ($current[0] - 1, $current[1]);
            } elsif $direction eq 'R' {
                $current = ($current[0] + 1, $current[1]);
            }
            %points{$current} //= $steps;
        }
    }
    return %points;
}
