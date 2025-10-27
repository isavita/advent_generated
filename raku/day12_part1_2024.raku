
sub MAIN {
    my @grid = 'input.txt'.IO.lines;
    my $rows = @grid.elems;
    return if $rows == 0;
    my $cols = @grid[0].chars;

    my @visited;
    push @visited, [ False xx $cols ] for ^$rows;

    my $total = 0;
    for ^$rows X ^$cols -> ($r,$c) {
        next if @visited[$r][$c];
        my ($area,$perim) = probe(@grid,$r,$c,@visited);
        $total += $area * $perim;
    }
    say $total;
}

sub probe(@g,$r,$c,@v) {
    my $ch = @g[$r].substr($c,1);
    my int $area  = 0;
    my int $perim = 0;
    my @queue = ($r,$c);
    @v[$r][$c] = True;

    while @queue {
        my $x = @queue.shift;
        my $y = @queue.shift;
        $area++;
        my $border = $x==0 || $x==@g.end || $y==0 || $y==@g[0].chars-1;

        for (-1,0),(1,0),(0,-1),(0,1) -> ($dx,$dy) {
            my ($nx,$ny) = ($x+$dx,$y+$dy);
            if $nx < 0 || $nx > @g.end || $ny < 0 || $ny > @g[0].chars-1 {
                $perim++ if $border;
                next;
            }
            if @g[$nx].substr($ny,1) ne $ch {
                $perim++;
            } elsif !@v[$nx][$ny] {
                @v[$nx][$ny] = True;
                @queue.append: $nx,$ny;
            }
        }
    }
    $area, $perim
}
