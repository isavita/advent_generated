
my $total = 0;
for slurp("input.txt").split("\n\n") -> $chunk {
    my ($ax,$ay,$bx,$by,$px,$py) = $chunk.comb(/ \-? \d+ /);
    my $best = Inf;
    for 0..100 -> $a {
        my $dx = $px - $ax*$a;
        my $dy = $py - $ay*$a;
        next unless $dx % $bx == 0 && $dy % $by == 0 && ($dx div $bx) == ($dy div $by);
        my $b = $dx div $bx;
        $best min= 3*$a + $b;
    }
    $total += $best unless $best == Inf;
}
say $total;
