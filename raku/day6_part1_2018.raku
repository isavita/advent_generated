
sub MAIN {
    my @coords = "input.txt".IO.lines.map({
        my ($x,$y) = .split(',').map(*.trim.Int);
        [$x,$y]
    });
    my $n = @coords.elems;
    my $minX = @coords.map(*[0]).min;
    my $maxX = @coords.map(*[0]).max;
    my $minY = @coords.map(*[1]).min;
    my $maxY = @coords.map(*[1]).max;
    my @area = (0) xx $n;
    my @inf  = (0) xx $n;
    for $minX .. $maxX -> $x {
        for $minY .. $maxY -> $y {
            my $minDist = Inf;
            my $closest = -1;
            my $total = 0;
            for @coords.kv -> $i, $c {
                my $d = ($x - $c[0]).abs + ($y - $c[1]).abs;
                $total += $d;
                if $d < $minDist {
                    $minDist = $d;
                    $closest = $i;
                } elsif $d == $minDist {
                    $closest = -1;
                }
            }
            if $closest != -1 {
                @area[$closest]++;
                if $x == $minX || $x == $maxX || $y == $minY || $y == $maxY {
                    @inf[$closest] = 1;
                }
            }
            # region size for part 2 (not printed)
        }
    }
    my $max = 0;
    for 0 ..^ $n -> $i {
        $max = @area[$i] if @area[$i] > $max && !@inf[$i];
    }
    say $max;
}
