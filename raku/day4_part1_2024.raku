
sub main {
    my @grid = 'input.txt'.IO.lines.map(*.trim);

    my $r = @grid.elems;
    my $c = @grid[0].chars;

    my $word = "XMAS";
    my $L = $word.chars;

    my @dirs = [[1,0], [-1,0], [0,1], [0,-1], [1,1], [1,-1], [-1,1], [-1,-1]];

    my $count = 0;

    for 0 ..^ $r -> $i {
        for 0 ..^ $c -> $j {
            if @grid[$i].substr($j, 1) eq $word.substr(0, 1) {
                for @dirs -> @dir {
                    my ($dx, $dy) = @dir;

                    my ($x, $y) = ($i, $j);
                    my $k = 0;

                    while $x >= 0 && $x < $r && $y >= 0 && $y < $c && $k < $L && @grid[$x].substr($y, 1) eq $word.substr($k, 1) {
                        $x += $dx;
                        $y += $dy;
                        $k++;
                    }

                    if $k == $L {
                        $count++;
                    }
                }
            }
        }
    }

    say $count;
}

main();
