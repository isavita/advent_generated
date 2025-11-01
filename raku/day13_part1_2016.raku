
unit sub MAIN {
    my $n = 'input.txt'.IO.slurp.trim.Int;
    my @q = ((1,1), 0);
    my %seen = (1,1 => 1);
    while @q {
        my ($x, $y, $d) = @q.shift;
        if $x == 31 && $y == 39 { say $d; exit }
        for (-1,1,0,0) Z (0,0,-1,1) -> ($dx,$dy) {
            my ($nx,$ny) = $x+$dx, $y+$dy;
            next if $nx < 0 or $ny < 0;
            my $bits = ( ($nx*$nx + 3*$nx + 2*$nx*$ny + $ny + $ny*$ny + $n).base(2).comb.sum );
            next if $bits % 2 or %seen{"$nx,$ny"}++;
            @q.push: ($nx,$ny,$d+1);
        }
    }
}
