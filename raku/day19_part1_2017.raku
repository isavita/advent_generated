
sub MAIN {
    my @grid = 'input.txt'.IO.lines;
    my $x    = @grid[0].index('|');
    my $y    = 0;
    my ($dx,$dy) = 0,1;
    my $out  = '';

    loop {
        last unless 0 ≤ $y < +@grid && 0 ≤ $x < @grid[$y].chars;
        my $c = @grid[$y].substr($x,1);
        last if $c eq ' ';
        $out ~= $c if $c ~~ /<[A..Z]>/;

        if $c eq '+' {
            ($dx,$dy) = $dx ?? (0,$y > 0 && @grid[$y-1].substr($x,1) ~~ /'|'|<[A..Z]>/ ?? -1 !! 1)
                           !! ((@grid[$y].substr($x-1,1) ~~ /'-'|<[A..Z]>/ ?? -1 !! 1),0);
        }
        $x += $dx;
        $y += $dy;
    }
    print $out;
}
