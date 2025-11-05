
sub parse-claim(Str $s) {
    my ($id, $left, $top, $width, $height) =
        $s.match(/^ '#' (\d+) \s+ '@' \s+ (\d+) ',' (\d+) ':' \s+ (\d+) 'x' (\d+) $/).list».Int;
    { id => $id, left => $left, top => $top, width => $width, height => $height }
}

sub MAIN {
    my @claims = 'input.txt'.IO.lines.map: { parse-claim($_) if /\S/ }
    my %fabric;
    for @claims -> $c {
        for $c<left> ..^ $c<left> + $c<width> -> $x {
            for $c<top> ..^ $c<top> + $c<height> -> $y {
                %fabric{"$x,$y"}++;
            }
        }
    }
    say %fabric.values.grep(* > 1).elems
}
