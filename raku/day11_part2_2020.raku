
sub MAIN {
    my @grid = 'input.txt'.IO.lines>>.comb;
    my \rows = @grid.elems;
    my \cols = @grid[0].elems;
    my @d = (-1,0,1) X (-1,0,1);
    @d .= grep: * !eqv (0,0);

    loop {
        my @next;
        my Bool $changed = False;

        for ^rows X ^cols -> (\r,\c) {
            my $cell = @grid[r][c];
            if $cell eq '.' {
                @next[r][c] = '.';
                next;
            }
            my $occ = 0;
            for @d -> (\dr,\dc) {
                my ($rr,$cc) = (r+dr,c+dc);
                while 0 <= $rr < rows && 0 <= $cc < cols {
                    my $seen = @grid[$rr][$cc];
                    if $seen eq '#' { $occ++; last }
                    if $seen eq 'L' { last }
                    $rr += dr; $cc += dc;
                }
            }
            if $cell eq 'L' && $occ == 0 {
                @next[r][c] = '#'; $changed = True
            }
            elsif $cell eq '#' && $occ >= 5 {
                @next[r][c] = 'L'; $changed = True
            }
            else {
                @next[r][c] = $cell
            }
        }
        @grid = @next;
        last unless $changed
    }
    say @grid.map(*.grep('#').elems).sum
}
