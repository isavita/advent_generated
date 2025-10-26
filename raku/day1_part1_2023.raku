
sub MAIN() {
    my $sum = 0;
    for 'input.txt'.IO.lines {
        my @d = .comb.grep: /\d/;
        $sum += 10 * @d[0] + @d[*-1] if @d;
    }
    say $sum;
}
