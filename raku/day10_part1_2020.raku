
sub MAIN() {
    my @a = "input.txt".IO.lines>>.Int.sort;
    my @d = 0, 0, 0, 1;
    my $p = 0;
    for @a -> $j { ++@d[$j - $p]; $p = $j }
    say @d[1] * @d[3];
}
