
sub MAIN {
    my @grid = 'input.txt'.IO.slurp.lines.map: -> $line { $line.comb(/\d/).map: *.Int };
    my $rows = @grid.elems;
    my $cols = @grid[0].elems;
    my %visible;

    for 0 .. $rows-1 -> $y {
        my $max-h = -1;
        for 0 .. $cols-1 -> $x {
            if @grid[$y][$x] > $max-h {
                %visible{$x => $y} = 1;
                $max-h = @grid[$y][$x];
            }
        }
    }

    for 0 .. $rows-1 -> $y {
        my $max-h = -1;
        for $cols-1 ... 0 -> $x {
            if @grid[$y][$x] > $max-h {
                %visible{$x => $y} = 1;
                $max-h = @grid[$y][$x];
            }
        }
    }

    for 0 .. $cols-1 -> $x {
        my $max-h = -1;
        for 0 .. $rows-1 -> $y {
            if @grid[$y][$x] > $max-h {
                %visible{$x => $y} = 1;
                $max-h = @grid[$y][$x];
            }
        }
    }

    for 0 .. $cols-1 -> $x {
        my $max-h = -1;
        for $rows-1 ... 0 -> $y {
            if @grid[$y][$x] > $max-h {
                %visible{$x => $y} = 1;
                $max-h = @grid[$y][$x];
            }
        }
    }

    say %visible.elems;
}
