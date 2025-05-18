
sub main {
    my @lines = 'input.txt'.IO.lines;
    my $height = @lines.elems;
    my $width = @lines[0].chars;

    my @galaxies;
    for @lines.kv -> $y, $line {
        for $line.comb.kv -> $x, $char {
            if $char eq '#' {
                @galaxies.push: [$x, $y];
            }
        }
    }

    my $galaxy-rows = Set.new(@galaxies.map(*[1]));
    my $galaxy-cols = Set.new(@galaxies.map(*[0]));

    my @empty-rows = (^$height).grep({ $_ ∉ $galaxy-rows });
    my @empty-cols = (^$width).grep({ $_ ∉ $galaxy-cols });

    my $expansion-factor = 1000000;
    my $expansion-delta = $expansion-factor - 1;

    my $total-distance = 0;

    for @galaxies.combinations(2) -> @pair {
        my ($g1-x, $g1-y) = @pair[0];
        my ($g2-x, $g2-y) = @pair[1];

        my ($x1, $x2) = sort $g1-x, $g2-x;
        my ($y1, $y2) = sort $g1-y, $g2-y;

        my $dist = ($x2 - $x1) + ($y2 - $y1);

        $dist += @empty-cols.grep({ $_ > $x1 && $_ < $x2 }).elems * $expansion-delta;
        $dist += @empty-rows.grep({ $_ > $y1 && $_ < $y2 }).elems * $expansion-delta;

        $total-distance += $dist;
    }

    say $total-distance;
}

main;
