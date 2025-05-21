
sub MAIN {
    my %firewall;
    for 'input.txt'.IO.lines -> $line {
        my ($depth, $rng) = $line.split(': ').map(*.Int);
        %firewall{$depth} = $rng;
    }

    my $severity = 0;
    my $max-depth = %firewall.keys.max // -1;

    for 0 .. $max-depth -> $time {
        if %firewall{$time}:exists {
            my $rng = %firewall{$time};
            my $scanner-pos;
            if $rng == 1 {
                $scanner-pos = 0;
            } else {
                my $cycle-len = ($rng - 1) * 2;
                $scanner-pos = $time % $cycle-len;
                if $scanner-pos >= $rng {
                    $scanner-pos = $cycle-len - $scanner-pos;
                }
            }
            if $scanner-pos == 0 {
                $severity += $time * $rng;
            }
        }
    }
    say $severity;
}
