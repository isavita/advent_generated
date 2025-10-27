
sub MAIN {
    my @stars = 'input.txt'.IO.lines.map: {
        /position\= '<' \s* (\-?\d+) ',' \s* (\-?\d+) '>' \s*
         velocity\= '<' \s* (\-?\d+) ',' \s* (\-?\d+) '>'/;
        { :x($0), :y($1), :vx($2), :vy($3) }
    }

    my $best-t = 0;
    my $min-area = Inf;

    for 1..100_000 -> $t {
        my $max-x = -Inf;
        my $max-y = -Inf;
        my $min-x =  Inf;
        my $min-y =  Inf;

        for @stars -> $s {
            my $x = $s<x> + $s<vx> * $t;
            my $y = $s<y> + $s<vy> * $t;
            $max-x = $x if $x > $max-x;
            $max-y = $y if $y > $max-y;
            $min-x = $x if $x < $min-x;
            $min-y = $y if $y < $min-y;
        }

        my $area = ($max-x - $min-x + 1) * ($max-y - $min-y + 1);
        if $area < $min-area {
            $min-area = $area;
            $best-t = $t;
        }
    }

    my $max-x = -Inf;
    my $max-y = -Inf;
    my $min-x =  Inf;
    my $min-y =  Inf;

    for @stars -> $s {
        $s<x> += $s<vx> * $best-t;
        $s<y> += $s<vy> * $best-t;
        $max-x = $s<x> if $s<x> > $max-x;
        $max-y = $s<y> if $s<y> > $max-y;
        $min-x = $s<x> if $s<x> < $min-x;
        $min-y = $s<y> if $s<y> < $min-y;
    }

    my %grid;
    for @stars -> $s {
        %grid{"$s<x>,$s<y>"} = '#';
    }

    for $min-y..$max-y -> $y {
        put join '',
            ($min-x..$max-x).map: { %grid{"$_,$y"} // ' ' }
    }
}
