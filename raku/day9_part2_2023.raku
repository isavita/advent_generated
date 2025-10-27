
sub MAIN() {
    my $total = 0;
    for 'input.txt'.IO.lines {
        my @hist = .comb(/\-?\d+/);
        my @first;
        while @hist {
            @first.unshift: @hist[0];
            @hist = (1..^@hist).map: { @hist[$_] - @hist[$_-1] };
            last if @hist.all == 0;
        }
        my $pred = 0;
        $pred = @first[$_] - $pred for ^@first;
        $total += $pred;
    }
    say $total;
}
