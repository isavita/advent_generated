
sub MAIN {
    my @a = 0, |slurp('input.txt').lines».Int;
    @a.=sort;
    @a.push: @a[*-1] + 3;

    my @w = 1;
    for 1..^@a -> $i {
        @w[$i] = 0;
        for 1..3 -> $j {
            @w[$i] += @w[$i - $j] if $i - $j >= 0 && @a[$i] - @a[$i - $j] <= 3;
        }
    }
    say @w[*-1];
}
