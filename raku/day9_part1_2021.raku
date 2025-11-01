
sub MAIN {
    my @map = 'input.txt'.IO.lines».comb».Int;
    my $risk = 0;
    for ^@map -> $y {
        for ^@map[0] -> $x {
            my $h = @map[$y][$x];
            next if $x > 0     && @map[$y][$x-1] <= $h;
            next if $x < @map[0]-1 && @map[$y][$x+1] <= $h;
            next if $y > 0     && @map[$y-1][$x] <= $h;
            next if $y < @map-1 && @map[$y+1][$x] <= $h;
            $risk += 1 + $h;
        }
    }
    say $risk;
}
