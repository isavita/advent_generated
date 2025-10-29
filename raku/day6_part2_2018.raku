
sub manhattan($x1,$y1,$x2,$y2) { abs($x1-$x2) + abs($y1-$y2) }

sub MAIN() {
    my @coords = "input.txt".IO.lines.map({ .split(',').map(*.trim.Int) });
    my $minX = @coords.map(*[0]).min;
    my $maxX = @coords.map(*[0]).max;
    my $minY = @coords.map(*[1]).min;
    my $maxY = @coords.map(*[1]).max;
    my $threshold = 10_000;
    my $safe = 0;
    for $minX .. $maxX -> $i {
        for $minY .. $maxY -> $j {
            my $total = [+] @coords.map({ manhattan($i,$j,$_[0],$_[1]) });
            $safe++ if $total < $threshold;
        }
    }
    say $safe;
}
