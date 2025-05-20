
sub MAIN() {
    my $total = 'input.txt'.IO.lines.map(*.Int).sum;
    say $total;
}
