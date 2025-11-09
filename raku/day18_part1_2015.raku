
constant $grid-size = 100;
constant $steps = 100;

sub next-state(@grid) {
    my @new = [[False xx $grid-size] xx $grid-size];
    for ^$grid-size -> $x {
        for ^$grid-size -> $y {
            my $count = 0;
            for -1..1 -> $dx {
                for -1..1 -> $dy {
                    next if $dx == 0 && $dy == 0;
                    my ($nx, $ny) = $x + $dx, $y + $dy;
                    $count++ if 0 <= $nx < $grid-size && 0 <= $ny < $grid-size && @grid[$nx][$ny];
                }
            }
            @new[$x][$y] = @grid[$x][$y] ?? $count == 2 || $count == 3 !! $count == 3;
        }
    }
    @new
}

sub count-lights-on(@grid) {
    @grid.map(*.sum).sum
}

sub MAIN() {
    my @grid = slurp('input.txt').lines.grep(*.trim).map({ [.comb.map({ $_ eq '#' })] });
    @grid = next-state(@grid) for ^$steps;
    say "Lights on after $steps steps: {count-lights-on(@grid)}";
}
