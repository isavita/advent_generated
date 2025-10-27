
sub MAIN {
    my @grid = 'input.txt'.IO.lines;
    my $h = @grid.elems;
    my $w = $h ?? @grid[0].chars !! 0;
    unless $h && $w { say 0; exit }

    my %coords;
    for ^$h X ^$w -> ($y,$x) {
        my $c = @grid[$y].substr($x,1);
        next if $c eq '.';
        %coords{$c}.push: ($y,$x);
    }

    my uint64 $antinodes = 0;
    my $inside = -> $y,$x { 0 <= $y < $h && 0 <= $x < $w };
    my %seen;

    for %coords.values -> @ant {
        next if @ant < 2;
        for @ant X @ant -> (($y1,$x1), ($y2,$x2)) {
            next if $y1 == $y2 && $x1 == $x2;
            my int $dy = $y2 - $y1;
            my int $dx = $x2 - $x1;
            my int $g  = $dy gcd $dx;
            $dy div= $g;
            $dx div= $g;
            if $dx < 0 || ($dx == 0 && $dy < 0) {
                $dy = -$dy;
                $dx = -$dx;
            }
            my int $c = $dy*$x1 - $dx*$y1;
            if $dy == 0 {
                my int $y = -$c div $dx;
                next unless $inside($y,0);
                loop (my int $x = 0; $x < $w; ++$x) {
                    my $k = "$y,$x";
                    next if %seen{$k};
                    %seen{$k} = 1;
                    ++$antinodes;
                }
            }
            elsif $dx == 0 {
                my int $x = $c div $dy;
                next unless $inside(0,$x);
                loop (my int $y = 0; $y < $h; ++$y) {
                    my $k = "$y,$x";
                    next if %seen{$k};
                    %seen{$k} = 1;
                    ++$antinodes;
                }
            }
            else {
                loop (my int $y = 0; $y < $h; ++$y) {
                    my int $num = $c + $dx*$y;
                    next unless $num %% $dy;
                    my int $x = $num div $dy;
                    next unless $inside($y,$x);
                    my $k = "$y,$x";
                    next if %seen{$k};
                    %seen{$k} = 1;
                    ++$antinodes;
                }
            }
        }
    }
    say $antinodes;
}
