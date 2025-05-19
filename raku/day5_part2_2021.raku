
sub main {
    my @lines = 'input.txt'.IO.lines.map({
        my ($start, $end) = $_.split(" -> ");
        my ($x1, $y1) = $start.split(",").map(*.Int);
        my ($x2, $y2) = $end.split(",").map(*.Int);
        [$x1, $y1, $x2, $y2];
    });

    my %overlaps;

    for @lines -> ($x1, $y1, $x2, $y2) {
        my $xStep = ($x2 <=> $x1);
        my $yStep = ($y2 <=> $y1);
        my $steps = max(abs($x2 - $x1), abs($y2 - $y1)) + 1;

        for 0 ..^ $steps -> $i {
            %overlaps{"{$x1 + $i * $xStep},{$y1 + $i * $yStep}"}++;
        }
    }

    say %overlaps.values.grep(* > 1).elems;
}

main();
