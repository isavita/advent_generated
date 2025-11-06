
sub MAIN {
    my ($depth, $target) = slurp('input.txt').lines.map: {
        when /depth/ { .words[1].Int }
        when /target/ { .split(':')[1].comb(/\d+/)».Int.List }
    }

    my @cave;
    @cave[0][0] = 0;
    my $tx = $target[0];
    my $ty = $target[1];

    for 0..$ty -> $y {
        for 0..$tx -> $x {
            my $geo = ($x == 0 && $y == 0) || ($x == $tx && $y == $ty)  ?? 0
                     !! $y == 0                                        ?? $x * 16807
                     !! $x == 0                                        ?? $y * 48271
                     !! @cave[$y][$x-1] * @cave[$y-1][$x];
            @cave[$y][$x] = ($geo + $depth) % 20183;
        }
    }

    my $risk = 0;
    for 0..$ty -> $y {
        for 0..$tx -> $x {
            $risk += @cave[$y][$x] % 3;
        }
    }
    say $risk;
}
