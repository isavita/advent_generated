
sub MAIN {
    my @grid = 'input.txt'.IO.lines».comb».Int;
    my \rows = @grid.elems;
    my \cols = @grid[0].elems;
    my @dp = [ -1 xx cols ] xx rows;

    sub dfs(Int $r, Int $c --> Int) {
        return @dp[$r][$c] if @dp[$r][$c] ≠ -1;
        my \h = @grid[$r][$c];
        return @dp[$r][$c] = 1 if h == 9;
        my Int $sum = 0;
        for (1, 0), (-1, 0), (0, 1), (0, -1) -> (\dr, \dc) {
            my ($nr, $nc) = $r + dr, $c + dc;
            next unless 0 ≤ $nr < rows and 0 ≤ $nc < cols;
            $sum += dfs($nr, $nc) if @grid[$nr][$nc] == h + 1;
        }
        @dp[$r][$c] = $sum;
    }

    my Int $total = 0;
    for ^rows X ^cols -> ($r, $c) {
        $total += dfs($r, $c) if @grid[$r][$c] == 0;
    }
    say $total;
}
