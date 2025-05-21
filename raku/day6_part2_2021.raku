
sub MAIN {
    my @counts = (0) xx 9;
    for 'input.txt'.IO.slurp.split(',') -> $age-str {
        @counts[$age-str.Int]++;
    }

    for 1..256 {
        my $reproducing = @counts[0];
        for 0..7 -> $j {
            @counts[$j] = @counts[$j+1];
        }
        @counts[6] += $reproducing;
        @counts[8] = $reproducing;
    }

    say @counts.sum;
}
