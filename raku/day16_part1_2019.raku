
sub MAIN {
    my @signal = slurp('input.txt').comb.map(*.Int);

    for ^100 {
        my @output = [0] xx @signal.elems;
        my @base-pattern = (0, 1, 0, -1);

        for ^@signal.elems -> $i {
            my $current-sum = 0;
            for ^@signal.elems -> $j {
                $current-sum += @signal[$j] * @base-pattern[ ( ($j + 1) div ($i + 1) ) % 4 ];
            }
            @output[$i] = $current-sum.abs % 10;
        }
        @signal = @output;
    }

    say @signal[0..7].join;
}
