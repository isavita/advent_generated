
sub filter-ratings(@candidates, &criteria) {
    my @current = @candidates;
    my $len = @current[0].chars;

    for 0 .. $len - 1 -> $i {
        my @zeros = @current.grep: *.substr($i, 1) eq '0';
        my @ones  = @current.grep: *.substr($i, 1) eq '1';

        my $keep = &criteria(@zeros.elems, @ones.elems);

        @current = ($keep eq '0') ?? @zeros !! @ones;

        last if @current.elems == 1;
    }
    return @current[0];
}

sub MAIN {
    my @values = 'input.txt'.IO.lines.map: *.chomp;

    my &oxygen-criteria = -> $zeros, $ones { $ones >= $zeros ?? '1' !! '0' };
    my &co2-criteria    = -> $zeros, $ones { $zeros <= $ones ?? '0' !! '1' };

    my $oxygen-rating-str = filter-ratings(@values, &oxygen-criteria);
    my $co2-rating-str    = filter-ratings(@values, &co2-criteria);

    my $oxygen-rating-int = :2($oxygen-rating-str);
    my $co2-rating-int    = :2($co2-rating-str);

    say $oxygen-rating-int * $co2-rating-int;
}
