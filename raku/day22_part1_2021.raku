
sub MAIN {
    my @steps = 'input.txt'.IO.lines.grep(*.so).map: -> $l {
        my ($a,$r) = $l.split(' ');
        my ($x,$y,$z) = $r.split(',').map: *.substr(2).split('..').map: *.Int;
        %( act => $a eq 'on', x0 => $x[0], x1 => $x[1], y0 => $y[0], y1 => $y[1], z0 => $z[0], z1 => $z[1] )
    }

    my %on;
    for @steps -> $s {
        next if $s<x0 x1 y0 y1 z0 z1>.any < -50 or $s<x0 x1 y0 y1 z0 z1>.any > 50;
        for max($s<x0>, -50)..min($s<x1>, 50) -> $x {
            for max($s<y0>, -50)..min($s<y1>, 50) -> $y {
                for max($s<z0>, -50)..min($s<z1>, 50) -> $z {
                    %on{"$x,$y,$z"} = $s<act>
                }
            }
        }
    }
    say %on.values.grep(*.so).elems
}
