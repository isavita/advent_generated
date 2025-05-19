
sub MAIN() {
    my @grid = 'input.txt'.IO.lines;
    my $r = @grid.elems;
    my $c = @grid[0].chars;
    my $count = 0;

    for 1 .. $r - 2 -> $x {
        for 1 .. $c - 2 -> $y {
            if @grid[$x].substr($y, 1) eq "A" {
                my $c11 = @grid[$x-1].substr($y-1, 1);
                my $c13 = @grid[$x-1].substr($y+1, 1);
                my $c22 = @grid[$x].substr($y, 1);
                my $c31 = @grid[$x+1].substr($y-1, 1);
                my $c33 = @grid[$x+1].substr($y+1, 1);

                my $d1 = $c11 ~ $c22 ~ $c33;
                my $d2 = $c13 ~ $c22 ~ $c31;

                if ($d1 eq "MAS" | "SAM") && ($d2 eq "MAS" | "SAM") {
                    $count++;
                }
            }
        }
    }

    say $count;
}
