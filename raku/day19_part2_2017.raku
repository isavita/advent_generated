
sub MAIN() {
    my @grid = 'input.txt'.IO.lines;
    my $height = @grid.elems;
    my $width = @grid[0].chars;

    my ($x, $y) = (0, 0);
    my ($dx, $dy) = (0, 1);
    my $steps = 0;

    $x = @grid[0].index('|');

    loop {
        my $cell = @grid[$y].substr($x, 1);

        last if $cell eq ' ';

        if $cell eq '+' {
            if $dx == 0 {
                if $x > 0 && @grid[$y].substr($x - 1, 1) eq '-' {
                    $dx = -1; $dy = 0;
                } else {
                    $dx = 1; $dy = 0;
                }
            } elsif $dy == 0 {
                if $y > 0 && @grid[$y - 1].substr($x, 1) eq '|' {
                    $dx = 0; $dy = -1;
                } else {
                    $dx = 0; $dy = 1;
                }
            }
        }

        $x += $dx;
        $y += $dy;
        $steps++;
    }

    say $steps;
}
