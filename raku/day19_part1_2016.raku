
sub MAIN() {
    my $n = 'input.txt'.IO.slurp.Int;
    my $p = 2 ** $n.msb;
    my $result = ($n - $p) * 2 + 1;
    say $result;
}
