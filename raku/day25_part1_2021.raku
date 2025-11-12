
sub MAIN {
    my @grid = 'input.txt'.IO.lines.map: *.comb.Array;
    my $step = 0;
    loop {
        my $east = move-east(@grid);
        my $south = move-south(@grid);
        $step++;
        last unless $east || $south;
    }
    say $step;
}

sub move-east(@grid) {
    my \h = @grid.elems;
    my \w = @grid[0].elems;
    my $moved = False;
    my @mark = @grid.map: *.clone;
    for ^h -> \y {
        for ^w -> \x {
            next unless @grid[y][x] eq '>';
            my \nx = (x + 1) % w;
            if @grid[y][nx] eq '.' {
                @mark[y][x] = '.';
                @mark[y][nx] = '>';
                $moved = True;
            }
        }
    }
    @grid = @mark if $moved;
    $moved
}

sub move-south(@grid) {
    my \h = @grid.elems;
    my \w = @grid[0].elems;
    my $moved = False;
    my @mark = @grid.map: *.clone;
    for ^w -> \x {
        for ^h -> \y {
            next unless @grid[y][x] eq 'v';
            my \ny = (y + 1) % h;
            if @grid[ny][x] eq '.' {
                @mark[y][x] = '.';
                @mark[ny][x] = 'v';
                $moved = True;
            }
        }
    }
    @grid = @mark if $moved;
    $moved
}
