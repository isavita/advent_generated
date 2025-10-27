
sub MAIN {
    my $sum = 0;
    for 'input.txt'.IO.lines {
        my @n = .words».Int;
        $sum += @n.max - @n.min
    }
    say $sum
}
