
sub MAIN {
    my @grid = 'input.txt'.IO.lines.map: *.comb;
    my $rows = @grid.elems;
    my $cols = @grid[0].elems;
    my $best = 0;

    for 0..^$rows -> $y {
        for 0..^$cols -> $x {
            next unless @grid[$y][$x] eq '#';
            my SetHash $seen .= new;
            for 0..^$rows -> $oy {
                for 0..^$cols -> $ox {
                    next unless @grid[$oy][$ox] eq '#';
                    next if $ox == $x && $oy == $y;
                    my ($dx, $dy) = $ox - $x, $oy - $y;
                    my $gcd = $dx gcd $dy;
                    $seen{"{$dx/$gcd},{$dy/$gcd}"} = True;
                }
            }
            $best = $seen.elems if $seen.elems > $best;
        }
    }
    say $best;
}
