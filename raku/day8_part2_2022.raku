
sub MAIN {
    my @grid;
    for "input.txt".IO.lines -> $line {
        @grid.push: $line.comb.map(*.Int);
    }

    my $rows = @grid.elems;
    my $cols = @grid[0].elems;

    my @neighbors = ([0, 1], [0, -1], [1, 0], [-1, 0]);

    my $max-score = 0;

    for ^$rows -> $y {
        for ^$cols -> $x {
            my $current-tree-height = @grid[$y][$x];
            my $score = 1;

            for @neighbors -> $delta {
                my ($dx, $dy) = $delta[0], $delta[1];
                my ($nx, $ny) = ($x, $y);
                my $view = 0;

                loop {
                    $nx += $dx;
                    $ny += $dy;

                    if $nx >= 0 and $nx < $cols and $ny >= 0 and $ny < $rows {
                        $view++;
                        if @grid[$ny][$nx] >= $current-tree-height {
                            $score *= $view;
                            last;
                        }
                    } else {
                        $score *= $view;
                        last;
                    }
                }
            }
            $max-score = $score if $score > $max-score;
        }
    }
    say $max-score;
}
