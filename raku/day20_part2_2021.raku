
sub MAIN {
    my ($alg, @img) = 'input.txt'.IO.slurp.trim.split("\n\n");
    $alg .= trim;
    my @in = @img.lines.map: { .comb».&{ $_ eq '#' } };
    my ($w, $h) = @in[0].elems, @in.elems;

    for ^50 -> $step {
        my $inf = $step % 2 && $alg.substr(0,1) eq '#' ?? 1 !! 0;
        my @out;
        for -1..$h -> $y {
            my @row;
            for -1..$w -> $x {
                my $idx = 0;
                for -1..1 -> $dy {
                    for -1..1 -> $dx {
                        $idx +<= 1;
                        my ($ny,$nx) = $y+$dy,$x+$dx;
                        $idx += ($ny ~~ 0..^$h && $nx ~~ 0..^$w
                                  ?? @in[$ny][$nx]
                                  !! $inf);
                    }
                }
                @row.push: $alg.substr($idx,1) eq '#';
            }
            @out.push: @row;
        }
        @in = @out;
        $w += 2; $h += 2;
    }
    say @in.map(*.sum).sum;
}
