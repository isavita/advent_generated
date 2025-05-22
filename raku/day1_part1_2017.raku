
sub MAIN() {
    my @chars = 'input.txt'.IO.slurp.trim.comb;
    my $sum = 0;
    for @chars Z @chars.rotate(1) -> ($a, $b) {
        $sum += $a.Int if $a eq $b;
    }
    say $sum;
}
