
sub get-mirror-axis-with-one-smudge(@lines) {
    for 1..^@lines -> $i {
        my $smudges = 0;
        my Bool $mirror = True;
        my $min = $i min @lines - $i;
        for ^$min -> $j {
            if @lines[$i-1-$j] +^ @lines[$i+$j] -> $diff {
                if $smudges || ($diff +& ($diff - 1)) {
                    $mirror = False;
                    last
                }
                $smudges++;
            }
        }
        return $i if $mirror && $smudges == 1;
    }
    0
}

sub MAIN {
    my $txt = slurp 'input.txt';
    my $res = 0;

    for $txt.split("\n\n") -> $block {
        my @grid = $block.lines».comb».cache;
        my $cols = @grid[0].elems;
        my $rows = @grid.elems;

        my @row-bits = (^$rows).map: -> $y {
            [+^] (@grid[$y].kv.map: { $^v eq '#' ?? 1 +< $^k !! 0 })
        }
        my @col-bits = (^$cols).map: -> $x {
            [+^] (@grid.map: { $_[$x] }).kv.map: { $^v eq '#' ?? 1 +< $^k !! 0 }
        }

        $res += get-mirror-axis-with-one-smudge(@col-bits);
        $res += 100 * get-mirror-axis-with-one-smudge(@row-bits);
    }

    say $res;
}
