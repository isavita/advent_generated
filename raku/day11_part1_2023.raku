
sub MAIN {
    my @lines = 'input.txt'.IO.lines;
    my $H = +@lines;
    my $W = @lines[0].chars;

    my @gal;
    for ^$H -> $y {
        for ^$W -> $x {
            @gal.push: ($x, $y) if @lines[$y].substr($x,1) eq '#';
        }
    }

    my @emptyR = (^$H).grep: -> $y { none @lines[$y].comb.map: * eq '#' };
    my @emptyC = (^$W).grep: -> $x { none (^$H).map: -> $y { @lines[$y].substr($x,1) eq '#' } };

    sub dist($a, $b, $factor) {
        my ($x1,$y1) = @$a;
        my ($x2,$y2) = @$b;
        my $dx = ($x1 - $x2).abs;
        my $dy = ($y1 - $y2).abs;
        $dx += ($factor-1) * +(@emptyC.grep: * ~~ ($x1 min $x2)..($x1 max $x2));
        $dy += ($factor-1) * +(@emptyR.grep: * ~~ ($y1 min $y2)..($y1 max $y2));
        $dx + $dy
    }

    my $total = 0;
    for ^@gal -> $i {
        for $i^..^@gal -> $j {
            $total += dist(@gal[$i], @gal[$j], 2)
        }
    }
    say $total
}
