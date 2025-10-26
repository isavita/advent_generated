
sub MAIN() {
    my @grid = 'input.txt'.IO.slurp.lines».comb;
    my ($h,$w) = @grid.elems, @grid[0].elems;

    my ($x,$y);
    for ^$h X ^$w -> ($i,$j) {
        if @grid[$i][$j] eq '^' { ($x,$y) = ($j,$i); last }
    }

    my @d = ((0,-1),(1,0),(0,1),(-1,0));
    my $dir = 0;
    my %seen = ($x,$y) => 1;

    loop {
        my ($dx,$dy) = @d[$dir];
        my ($nx,$ny) = ($x+$dx, $y+$dy);

        last if $nx < 0 or $nx >= $w or $ny < 0 or $ny >= $h;

        if @grid[$ny][$nx] eq '#' {
            $dir = ($dir + 1) % 4;
            next;
        }

        ($x,$y) = ($nx,$ny);
        %seen{"$x,$y"} = 1;
    }

    say %seen.elems
}
