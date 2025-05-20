
sub MAIN {
    my @numbers = 'input.txt'.IO.lines.map(*.Int);
    my $invalid-num = 14360655;

    for 0 .. @numbers.end -> $i {
        my $total = 0;
        my $j = $i;
        while $total < $invalid-num && $j < @numbers.elems {
            $total += @numbers[$j];
            $j++;
        }

        if $total == $invalid-num {
            my @contiguous-set = @numbers[$i .. $j - 1];
            my $weakness = @contiguous-set.min + @contiguous-set.max;
            say $weakness;
            last;
        }
    }
}
