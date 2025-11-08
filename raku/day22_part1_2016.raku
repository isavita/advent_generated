
sub MAIN {
    my @nodes = 'input.txt'.IO.lines.skip(2).map: {
        my ($x,$y,$size,$used,$avail) = $_.match(
            /node '-' x(\d+) '-' y(\d+) \s+ (\d+) 'T' \s+ (\d+) 'T' \s+ (\d+) 'T'/
        )[0..4];
        { :$x, :$y, :$size, :$used, :$avail }
    }

    my $count = 0;
    for @nodes.kv -> $i, $a {
        for @nodes.kv -> $j, $b {
            ++$count if $i != $j && $a<used> > 0 && $a<used> <= $b<avail>;
        }
    }

    say $count;
}
