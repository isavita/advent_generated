
constant GRID_SIZE = 71;

sub bfs(@grid --> Int) {
    return -1 if @grid[0][0] eq '#';
    return -1 if @grid[GRID_SIZE-1][GRID_SIZE-1] eq '#';
    my @visited = (0 ..^ GRID_SIZE).map({ [False xx GRID_SIZE] }).Array;
    my @queue = ((0, 0, 0),);
    @visited[0][0] = True;
    my @dr = (0, 0, 1, -1);
    my @dc = (1, -1, 0, 0);
    while @queue {
        my ($r, $c, $dist) = @queue.shift;
        if $r == GRID_SIZE-1 && $c == GRID_SIZE-1 {
            return $dist;
        }
        for ^4 -> $i {
            my $nr = $r + @dr[$i];
            my $nc = $c + @dc[$i];
            if $nr >= 0 && $nr < GRID_SIZE && $nc >= 0 && $nc < GRID_SIZE && @grid[$nr][$nc] eq '.' && !@visited[$nr][$nc] {
                @visited[$nr][$nc] = True;
                @queue.push(($nr, $nc, $dist + 1));
            }
        }
    }
    return -1;
}

sub MAIN() {
    my @positions = "input.txt".IO.lines.map({ [ .split(',')>>.Int ] }).Array;
    my @grid1 = (0 ..^ GRID_SIZE).map({ ['.' xx GRID_SIZE] }).Array;
    my $limit = @positions > 1024 ?? 1024 !! @positions.elems;
    for ^$limit -> $i {
        my ($x, $y) = @positions[$i];
        @grid1[$y][$x] = '#';
    }
    say bfs(@grid1);

    my @grid2 = (0 ..^ GRID_SIZE).map({ ['.' xx GRID_SIZE] }).Array;
    for @positions -> $p {
        my ($x, $y) = $p;
        @grid2[$y][$x] = '#';
        my $d = bfs(@grid2);
        if $d == -1 {
            say "$x,$y";
            last;
        }
    }
}
